package Impl


import Common.API.{PlanContext, Planner}
import Common.DBAPI._
import Common.Object.SqlParameter
import Common.ServiceUtils.schemaName
import Objects.CourseManagementService.CourseInfo
import Objects.CourseManagementService.CourseTime
import Objects.CourseManagementService.DayOfWeek
import Objects.CourseManagementService.TimePeriod
import cats.effect.IO
import org.slf4j.LoggerFactory
import cats.implicits._
import Utils.CourseManagementProcess.validateTeacherToken
import Utils.CourseManagementProcess.fetchCourseByID
import Utils.CourseManagementProcess.validateTeacherManagePermission
import Utils.CourseManagementProcess.recordCourseManagementOperationLog
import io.circe._
import io.circe.syntax._
import io.circe.generic.auto._
import org.joda.time.DateTime
import cats.implicits.*
import Common.Serialize.CustomColumnTypes.{decodeDateTime, encodeDateTime}
import APIs.UserAuthService.VerifyTokenValidityMessage
import Objects.UserAccountService.SafeUserInfo
import Objects.UserAccountService.UserRole
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
import Common.Serialize.CustomColumnTypes.{decodeDateTime,encodeDateTime}

case class UpdateCourseMessagePlanner(
  teacherToken: String,
  courseID: Int,
  newCapacity: Option[Int],
  newLocation: Option[String],
  override val planContext: PlanContext
) extends Planner[CourseInfo] {

  val logger = LoggerFactory.getLogger(this.getClass.getSimpleName + "_" + planContext.traceID.id)

  override def plan(using PlanContext): IO[CourseInfo] = for {
    // Step 1: Validate teacher token and get teacherID
    _ <- IO(logger.info(s"验证教师 Token: ${teacherToken}"))
    teacherIDOpt <- validateTeacherToken(teacherToken)
    teacherID <- teacherIDOpt match {
      case Some(id) => IO.pure(id)
      case None =>
        val errMsg = s"Token 验证失败或用户不具有教师权限，Token: ${teacherToken}"
        logger.error(errMsg)
        IO.raiseError(new IllegalArgumentException(errMsg))
    }
    _ <- IO(logger.info(s"教师 ID 验证成功: ${teacherID}"))

    // Step 2: Fetch course by ID and check if course exists
    _ <- IO(logger.info(s"查询课程信息，课程 ID: ${courseID}"))
    courseOpt <- fetchCourseByID(courseID)
    course <- courseOpt match {
      case Some(c) if c.teacherID == teacherID => IO.pure(c)
      case Some(_) =>
        val errMsg = s"课程教师权限不足，课程 ID: ${courseID}，教师 ID: ${teacherID}"
        logger.error(errMsg)
        IO.raiseError(new IllegalStateException(errMsg))
      case None =>
        val errMsg = s"课程不存在，课程 ID: ${courseID}"
        logger.error(errMsg)
        IO.raiseError(new IllegalStateException(errMsg))
    }
    _ <- IO(logger.info(s"课程信息验证成功，课程 ID: ${course.courseID}, 教师 ID: ${course.teacherID}"))

    // Step 3: Validate permissions to manage courses
    _ <- IO(logger.info(s"验证教师修改课程权限"))
    canManage <- validateTeacherManagePermission()
    _ <- if (!canManage) {
      val errMsg = s"当前阶段不允许教师修改课程信息，教师 ID: ${teacherID}"
      logger.error(errMsg)
      IO.raiseError(new IllegalStateException(errMsg))
    } else {
      IO(logger.info(s"教师权限验证成功，允许修改课程"))
    }

    // Step 4: Update CourseTable with new values
    _ <- IO(logger.info(s"更新课程信息，课程 ID: ${courseID}, 新容量: ${newCapacity}, 新地点: ${newLocation}"))
    updateResult <- {
      val sql = s"""
        UPDATE ${schemaName}.course_table
        SET course_capacity = COALESCE(?, course_capacity),
            location = COALESCE(?, location)
        WHERE course_id = ?;
      """
      val parameters = List(
        SqlParameter("Int", newCapacity.getOrElse(course.courseCapacity).asJson.noSpaces),
        SqlParameter("String", newLocation.getOrElse(course.location)),
        SqlParameter("Int", courseID.asJson.noSpaces)
      )
      writeDB(sql, parameters)
    }
    _ <- IO(logger.info(s"课程信息更新成功，课程 ID: ${courseID}"))

    // Step 5: Record operation log
    operationDetails = s"修改课程容量为 ${newCapacity.getOrElse(course.courseCapacity)}，地点为 ${newLocation.getOrElse(course.location)}"
    _ <- recordCourseManagementOperationLog(
      teacherID = teacherID,
      operation = "修改课程",
      courseID = courseID,
      details = operationDetails
    )
    _ <- IO(logger.info(s"记录教师课程操作日志成功，课程 ID: ${courseID}"))

    // Step 6: Return updated course information
    updatedCourse = course.copy(
      courseCapacity = newCapacity.getOrElse(course.courseCapacity),
      location = newLocation.getOrElse(course.location)
    )
    _ <- IO(logger.info(s"返回更新后的课程信息: ID=${updatedCourse.courseID}, 容量=${updatedCourse.courseCapacity}, 地点=${updatedCourse.location}"))
  } yield updatedCourse
}