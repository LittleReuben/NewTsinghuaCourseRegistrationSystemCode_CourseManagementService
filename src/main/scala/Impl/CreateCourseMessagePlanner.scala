package Impl


import Common.API.{PlanContext, Planner}
import Common.DBAPI._
import Common.Object.SqlParameter
import Common.ServiceUtils.schemaName
import Objects.CourseManagementService.{CourseInfo, CourseTime}
import cats.effect.IO
import org.slf4j.LoggerFactory
import Utils.CourseManagementProcess._
import APIs.UserAuthService.VerifyTokenValidityMessage
import org.joda.time.DateTime
import io.circe._
import io.circe.syntax._
import io.circe.generic.auto._
import cats.implicits._
import Common.Serialize.CustomColumnTypes.{decodeDateTime, encodeDateTime}
import Utils.CourseManagementProcess.fetchCourseByID
import Objects.CourseManagementService.CourseTime
import Utils.CourseManagementProcess.recordCourseManagementOperationLog
import Utils.CourseManagementProcess.checkAuthorizationForTeacher
import Utils.CourseManagementProcess.validateTeacherManagePermission
import Objects.CourseManagementService.DayOfWeek
import Objects.CourseManagementService.TimePeriod
import Utils.CourseManagementProcess.validateCourseTimeConflict
import Objects.UserAccountService.SafeUserInfo
import Utils.CourseManagementProcess.validateTeacherToken
import Objects.UserAccountService.UserRole
import Objects.CourseManagementService.CourseInfo
import io.circe._
import io.circe.syntax._
import io.circe.generic.auto._
import org.joda.time.DateTime
import cats.implicits.*
import Common.DBAPI._
import Common.API.{PlanContext, Planner}
import cats.effect.IO
import Common.Object.SqlParameter
import Common.Serialize.CustomColumnTypes.{decodeDateTime,encodeDateTime}
import Common.ServiceUtils.schemaName
import Objects.CourseManagementService.CourseInfo
import cats.implicits.*
import Common.Serialize.CustomColumnTypes.{decodeDateTime,encodeDateTime}

case class CreateCourseMessagePlanner(
  teacherToken: String,
  courseGroupID: Int,
  courseCapacity: Int,
  time: List[CourseTime],
  location: String,
  override val planContext: PlanContext
) extends Planner[CourseInfo] {
  private val logger = LoggerFactory.getLogger(this.getClass.getSimpleName + "_" + planContext.traceID.id)

  override def plan(using PlanContext): IO[CourseInfo] = {
    for {
      // Step 1: 验证 teacherToken 并获取 teacherID
      teacherIDOpt <- validateTeacherToken(teacherToken)
      teacherID <- IO {
        teacherIDOpt.getOrElse(throw new IllegalStateException("教师鉴权失败。"))
      }
      _ <- IO(logger.info(s"验证教师Token成功，TeacherID=${teacherID}"))

      // Step 2: 验证老师是否被授权开设课程组
      isAuthorized <- checkAuthorizationForTeacher(teacherID, courseGroupID)
      _ <- if (!isAuthorized) IO.raiseError(new IllegalStateException(s"未获得课程组授权。")) else IO(logger.info("课程组授权验证通过"))

      // Step 3: 验证当前阶段是否允许创建课程
      managePermission <- validateTeacherManagePermission()
      _ <- if (!managePermission) IO.raiseError(new IllegalStateException(s"操作课程权限未开启！")) else IO(logger.info("权限验证通过，老师可以创建课程"))

      // Step 4: 检查新增课程时间是否冲突
      hasTimeConflict <- validateCourseTimeConflict(teacherID, time)
      _ <- if (hasTimeConflict) IO.raiseError(new IllegalStateException(s"课程时间冲突，无法创建课程。")) else IO(logger.info("课程时间验证通过，无时间冲突"))

      // Step 5: 将课程插入到数据库
      courseID <- insertCourse(teacherID)

      // Step 6: 记录创建课程的操作日志
      _ <- recordCourseManagementOperationLog(teacherID, "创建课程", courseID, s"课程地点：${location}，时间：${time}")

      // Step 7: 返回新增课程信息
      courseInfo <- fetchCourseByID(courseID).flatMap {
        case Some(course) => IO(course)
        case None => IO.raiseError(new IllegalStateException(s"课程信息无法获取，课程ID=${courseID}"))
      }
    } yield courseInfo
  }

  /**
   * 将课程信息插入数据库并返回对应的 CourseID。
   */
  private def insertCourse(teacherID: Int)(using PlanContext): IO[Int] = {
    val sql =
      s"""
INSERT INTO ${schemaName}.course_table
(course_group_id, course_capacity, time, location, teacher_id)
VALUES (?, ?, ?, ?, ?)
RETURNING course_id;
       """
    val parameters = List(
      SqlParameter("Int", courseGroupID.toString),
      SqlParameter("Int", courseCapacity.toString),
      SqlParameter("String", time.asJson.noSpaces),
      SqlParameter("String", location),
      SqlParameter("Int", teacherID.toString)
    )

    for {
      _ <- IO(logger.info(s"准备执行插入课程指令，SQL=${sql} 参数=${parameters.map(_.value).mkString(", ")}"))
      courseID <- readDBInt(sql, parameters)
      _ <- IO(logger.info(s"课程插入成功，生成的课程ID为${courseID}"))
    } yield courseID
  }
}