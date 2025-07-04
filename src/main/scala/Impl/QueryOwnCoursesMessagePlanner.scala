package Impl

import APIs.UserAuthService.VerifyTokenValidityMessage
import Objects.CourseManagementService.{CourseInfo, CourseTime, DayOfWeek, TimePeriod}
import Utils.CourseManagementProcess.validateTeacherToken
import Objects.UserAccountService.SafeUserInfo
import Objects.UserAccountService.UserRole
import Common.API.{PlanContext, Planner}
import Common.DBAPI._
import Common.Object.SqlParameter
import Common.ServiceUtils.schemaName
import cats.effect.IO
import org.slf4j.LoggerFactory
import org.joda.time.DateTime
import io.circe._
import io.circe.syntax._
import io.circe.generic.auto._
import cats.implicits._

case class QueryOwnCoursesMessagePlanner(
  teacherToken: String,
  override val planContext: PlanContext
) extends Planner[List[CourseInfo]] {

  private val logger = LoggerFactory.getLogger(this.getClass.getSimpleName + "_" + planContext.traceID.id)

  override def plan(using PlanContext): IO[List[CourseInfo]] = {
    for {
      // Step 1: Validate teacherToken
      _ <- IO(logger.info(s"验证教师 token: ${teacherToken}"))
      teacherIDOpt <- validateTeacherToken(teacherToken)
      teacherID <- teacherIDOpt match {
        case Some(id) => IO.pure(id)
        case None =>
          IO(logger.error("教师 token 鉴权失败，无法获取教师 ID")) >>
          IO.raiseError(new IllegalStateException("教师鉴权失败"))
      }
      _ <- IO(logger.info(s"教师 ID 验证成功: ${teacherID}"))

      // Step 2: Query courses by teacherID and fetch detailed information
      courseIDs <- queryCourseIDsByTeacherID(teacherID)
      _ <- IO(logger.info(s"查询课程记录完成，总数为 ${courseIDs.size}"))
      courses <- fetchCoursesByIDs(courseIDs)

    } yield courses
  }

  private def queryCourseIDsByTeacherID(teacherID: Int)(using PlanContext): IO[List[Int]] = {
    val sql =
      s"""
SELECT course_id
FROM ${schemaName}.course_table
WHERE teacher_id = ?;
         """.stripMargin
    logger.info(s"正在执行 SQL 查询: ${sql}")
    for {
      courseRows <- readDBRows(sql, List(SqlParameter("Int", teacherID.toString)))
      courseIDs <- IO {
        courseRows.map(json => decodeField[Int](json, "course_id"))
      }
    } yield courseIDs
  }

  private def fetchCoursesByIDs(courseIDs: List[Int])(using PlanContext): IO[List[CourseInfo]] = {
    logger.info("开始调用 fetchCourseByID 方法获取详细的课程信息")
    for {
      courseInfos <- IO(courseIDs.map(fetchCourseByID))
      result <- courseInfos.sequence
    } yield result
  }

  private def fetchCourseByID(courseID: Int)(using PlanContext): IO[CourseInfo] = {
    logger.info(s"正在调用 fetchCourseByID 获取 courseID: ${courseID} 的课程信息")
    Utils.CourseManagementProcess.fetchCourseByID(courseID).flatMap {
      case Some(courseInfo) => IO.pure(courseInfo)
      case None =>
        IO(logger.error(s"无法找到 courseID: ${courseID} 的课程信息")) >>
        IO.raiseError(new IllegalStateException(s"无法找到 courseID: ${courseID} 的课程信息"))
    }
  }
}