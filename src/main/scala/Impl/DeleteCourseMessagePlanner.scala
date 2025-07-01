package Impl


import Common.API.{PlanContext, Planner}
import Common.DBAPI._
import Common.Object.SqlParameter
import Common.ServiceUtils.schemaName
import Utils.CourseManagementProcess.{validateTeacherToken, validateTeacherManagePermission, recordCourseManagementOperationLog, fetchCourseByID}
import Objects.CourseManagementService.CourseInfo
import cats.effect.IO
import org.slf4j.LoggerFactory
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
import Utils.CourseManagementProcess.fetchCourseByID
import Objects.CourseManagementService.CourseTime
import Utils.CourseManagementProcess.recordCourseManagementOperationLog
import Utils.CourseManagementProcess.validateTeacherManagePermission
import Objects.CourseManagementService.DayOfWeek
import Objects.CourseManagementService.TimePeriod
import Objects.UserAccountService.SafeUserInfo
import Utils.CourseManagementProcess.validateTeacherToken
import Objects.UserAccountService.UserRole
import Objects.CourseManagementService.CourseInfo

case class DeleteCourseMessagePlanner(
                                       teacherToken: String,
                                       courseID: Int,
                                       override val planContext: PlanContext
                                     ) extends Planner[String] {
  val logger = LoggerFactory.getLogger(this.getClass.getSimpleName + "_" + planContext.traceID.id)

  override def plan(using PlanContext): IO[String] = {
    for {
      // Step 1: Validate teacherToken and retrieve teacherID
      _ <- IO(logger.info(s"开始验证教师 token: ${teacherToken}"))
      teacherIDOpt <- validateTeacherToken(teacherToken)
      teacherID <- IO {
        teacherIDOpt.getOrElse {
          val errorMsg = s"教师 token 验证失败或无效: ${teacherToken}"
          logger.error(errorMsg)
          throw new IllegalArgumentException(errorMsg)
        }
      }
      _ <- IO(logger.info(s"教师 token 验证通过，教师 ID: ${teacherID}"))

      // Step 2: Check if the course exists and matches the teacherID
      _ <- IO(logger.info(s"开始检查课程是否存在以及教师ID是否匹配，课程ID: ${courseID}, 教师ID: ${teacherID}"))
      courseOpt <- fetchCourseByID(courseID)
      course <- IO {
        courseOpt.getOrElse {
          val errorMsg = s"课程 ID=${courseID} 不存在"
          logger.error(errorMsg)
          throw new IllegalArgumentException(errorMsg)
        }
      }
      _ <- IO {
        if (course.teacherID != teacherID) {
          val errorMsg = s"教师 ID=${teacherID} 无权管理课程 ID=${courseID}"
          logger.error(errorMsg)
          throw new IllegalAccessException(errorMsg)
        } else {
          logger.info(s"教师 ID=${teacherID} 与课程 ID=${courseID} 匹配成功")
        }
      }

      // Step 3: Validate permission for teacher to delete course
      _ <- IO(logger.info(s"开始验证教师是否有权限删除课程"))
      permission <- validateTeacherManagePermission()
      _ <- IO {
        if (!permission) {
          val errorMsg = s"当前阶段不允许教师删除课程"
          logger.error(errorMsg)
          throw new IllegalAccessException(errorMsg)
        }
      }
      _ <- IO(logger.info(s"教师删除课程权限验证通过"))

      // Step 4: Delete the course record in the CourseTable
      _ <- IO(logger.info(s"开始删除课程记录，课程ID: ${courseID}"))
      deleteSql =
        s"""
           DELETE FROM ${schemaName}.course_table
           WHERE course_id = ?;
         """
      deleteResult <- writeDB(deleteSql, List(SqlParameter("Int", courseID.toString)))
      _ <- IO(logger.info(s"课程记录删除成功，结果: ${deleteResult}"))

      // Step 5: Record course management operation log
      _ <- IO(logger.info(s"开始记录课程删除操作日志"))
      logResult <- recordCourseManagementOperationLog(
        teacherID = teacherID,
        operation = "DELETE_COURSE",
        courseID = courseID,
        details = s"教师 ID=${teacherID} 删除了课程 ID=${courseID}"
      )
      _ <- IO(logger.info(s"课程删除操作日志记录成功，结果: ${logResult}"))

      // Step 6: Return success message
      result <- IO(logger.info("课程删除操作完成，返回确认信息")) >>
        IO.pure("删除成功！")
    } yield result
  }
}