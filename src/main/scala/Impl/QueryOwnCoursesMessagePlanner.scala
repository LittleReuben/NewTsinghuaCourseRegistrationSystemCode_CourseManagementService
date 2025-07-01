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
import cats.implicits.*
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
import Objects.CourseManagementService.CourseTime
import Objects.CourseManagementService.DayOfWeek
import Objects.CourseManagementService.TimePeriod
import Objects.CourseManagementService.CourseInfo
import io.circe.Json
import io.circe.parser._
import Common.Serialize.CustomColumnTypes.{decodeDateTime,encodeDateTime}
import Objects.CourseManagementService.CourseInfo

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

      // Step 2: Query courses from CourseTable
      courses <- queryCoursesByTeacherID(teacherID)
      _ <- IO(logger.info(s"查询课程记录完成，总数为 ${courses.size}"))

    } yield courses
  }

  private def queryCoursesByTeacherID(teacherID: Int)(using PlanContext): IO[List[CourseInfo]] = {
    val sql =
      s"""
SELECT course_id, course_capacity, time, location, course_group_id, teacher_id
FROM ${schemaName}.course_table
WHERE teacher_id = ?;
         """.stripMargin
    logger.info(s"正在执行 SQL 查询: ${sql}")
    for {
      courseRows <- readDBRows(sql, List(SqlParameter("Int", teacherID.toString)))
      courses <- IO {
        courseRows.map { json =>
          // 将查询结果转换为具体 CourseInfo 对象
          val courseID = decodeField[Int](json, "course_id")
          val courseCapacity = decodeField[Int](json, "course_capacity")
          val timeRaw = decodeField[String](json, "time")
          val location = decodeField[String](json, "location")
          val courseGroupID = decodeField[Int](json, "course_group_id")
          val teacherID = decodeField[Int](json, "teacher_id")

          // 解析时间字段
          val timeList = decodeType[List[CourseTime]](timeRaw)

          CourseInfo(
            courseID = courseID,
            courseCapacity = courseCapacity,
            time = timeList,
            location = location,
            courseGroupID = courseGroupID,
            teacherID = teacherID,
            preselectedStudentsSize = 0, // 初始化为0，数据库表中不存在此字段
            selectedStudentsSize = 0,   // 初始化为0，数据库表中不存在此字段
            waitingListSize = 0         // 初始化为0，数据库表中不存在此字段
          )
        }
      }
    } yield courses
  }
}