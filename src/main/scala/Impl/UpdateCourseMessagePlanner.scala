package Impl


import Common.API.{PlanContext, Planner}
import Common.DBAPI._
import Common.Object.SqlParameter
import Common.ServiceUtils.schemaName
import Objects.CourseManagementService.CourseInfo
import Objects.CourseManagementService.CourseTime
import Objects.CourseManagementService.DayOfWeek
import Objects.CourseManagementService.TimePeriod
import Objects.UserAccountService.UserRole
import Objects.UserAccountService.SafeUserInfo
import Utils.CourseManagementProcess.fetchCourseByID
import Utils.CourseManagementProcess.recordCourseManagementOperationLog
import Utils.CourseManagementProcess.validateTeacherManagePermission
import Utils.CourseManagementProcess.validateTeacherToken
import cats.effect.IO
import org.slf4j.LoggerFactory
import org.joda.time.DateTime
import io.circe._
import io.circe.syntax._
import io.circe.generic.auto._
import cats.implicits._
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
import APIs.UserAuthService.VerifyTokenValidityMessage
import Objects.CourseManagementService.CourseInfo
import Utils.CourseManagementProcess._
import cats.implicits.*
import Common.Serialize.CustomColumnTypes.{decodeDateTime,encodeDateTime}

case class UpdateCourseMessagePlanner(
  teacherToken: String,
  courseID: Int,
  newCapacity: Option[Int],
  newLocation: Option[String],
  override val planContext: PlanContext
) extends Planner[CourseInfo] {

  private val logger = LoggerFactory.getLogger(this.getClass.getSimpleName + "_" + planContext.traceID.id)

  override def plan(using PlanContext): IO[CourseInfo] = {
    for {
      // Step 1: 验证教师 token，并获取教师 ID
      _ <- IO(logger.info("[UpdateCourseMessagePlanner] 验证教师 token 的有效性"))
      teacherIDOption <- validateTeacherToken(teacherToken)
      teacherID <- IO.fromOption(teacherIDOption)(
        new IllegalArgumentException(s"无效的教师 token: $teacherToken")
      )
      _ <- IO(logger.info(s"[UpdateCourseMessagePlanner] 验证通过，教师 ID 为: $teacherID"))

      // Step 2: 查询课程信息并验证教师权限
      _ <- IO(logger.info("[UpdateCourseMessagePlanner] 查询课程信息"))
      courseOption <- fetchCourseByID(courseID)
      course <- IO.fromOption(courseOption)(
        new IllegalStateException(s"课程不存在，课程ID=$courseID")
      )
      _ <- IO {
        if (course.teacherID != teacherID) {
          throw new IllegalStateException(
            s"教师 ID 不匹配: 教师ID=$teacherID, 课程教师 ID=${course.teacherID}"
          )
        } else {
          logger.info("[UpdateCourseMessagePlanner] 课程教师 ID 匹配，继续处理")
        }
      }

      // Step 3: 验证当前是否允许修改课程
      _ <- IO(logger.info("[UpdateCourseMessagePlanner] 验证教师是否有管理权限"))
      allowManage <- validateTeacherManagePermission()
      _ <- if (!allowManage) {
        IO.raiseError(new IllegalStateException("当前阶段不允许教师修改课程"))
      } else IO.unit
      _ <- IO(logger.info("[UpdateCourseMessagePlanner] 验证通过，允许教师修改课程"))

      // Step 4: 更新课程容量和地点
      _ <- IO(logger.info("[UpdateCourseMessagePlanner] 准备更新课程容量和地点"))
      updateCapacitySQL = newCapacity.fold("course_capacity = course_capacity")(cap => s"course_capacity = $cap")
      updateLocationSQL = newLocation.fold("location = location")(loc => s"location = '$loc'")
      completeSQL = s"""
        UPDATE ${schemaName}.course_table
        SET $updateCapacitySQL, $updateLocationSQL
        WHERE course_id = ?;
      """
      updateParameters = List(SqlParameter("Int", courseID.toString))
      _ <- writeDB(completeSQL, updateParameters)
      _ <- IO(logger.info("[UpdateCourseMessagePlanner] 课程更新成功"))

      // Step 5: 记录操作日志
      operationDetails = s"更新课程信息: 容量=${newCapacity.getOrElse(course.courseCapacity)}, 地点=${newLocation.getOrElse(course.location)}"
      _ <- recordCourseManagementOperationLog(
        teacherID = teacherID,
        operation = "UpdateCourse",
        courseID = courseID,
        details = operationDetails
      )
      _ <- IO(logger.info("[UpdateCourseMessagePlanner] 操作日志记录成功"))

      // Step 6: 查询并返回更新后的课程信息
      _ <- IO(logger.info("[UpdateCourseMessagePlanner] 查询更新后的课程信息"))
      updatedCourseOption <- fetchCourseByID(courseID)
      updatedCourse <- IO.fromOption(updatedCourseOption)(
        new IllegalStateException(s"更新后的课程信息查询失败，课程ID=$courseID")
      )
      _ <- IO(logger.info(s"[UpdateCourseMessagePlanner] 返回更新后的课程信息: $updatedCourse"))

    } yield updatedCourse
  }
}