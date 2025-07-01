package Impl


import Common.API.{PlanContext, Planner}
import Common.DBAPI._
import Common.Object.SqlParameter
import Common.ServiceUtils.schemaName
import Objects.CourseManagementService.CourseInfo
import Objects.CourseManagementService.CourseTime
import Objects.CourseManagementService.DayOfWeek
import Objects.CourseManagementService.TimePeriod
import Utils.CourseManagementProcess.validateTeacherToken
import org.slf4j.LoggerFactory
import io.circe._
import io.circe.syntax._
import io.circe.generic.auto._
import cats.effect.IO
import cats.implicits.*
import Common.Serialize.CustomColumnTypes.{decodeDateTime, encodeDateTime}
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
import APIs.UserAuthService.VerifyTokenValidityMessage
import Objects.CourseManagementService.CourseInfo
import org.joda.time.DateTime
import Common.Serialize.CustomColumnTypes.{decodeDateTime,encodeDateTime}

case class QueryOwnCoursesMessagePlanner(
                                           teacherToken: String,
                                           override val planContext: PlanContext
                                         ) extends Planner[List[CourseInfo]] {
  val logger = LoggerFactory.getLogger(this.getClass.getSimpleName + "_" + planContext.traceID.id)

  override def plan(using PlanContext): IO[List[CourseInfo]] = {
    for {
      // Step 1: Validate the teacherToken and get teacherID
      _ <- IO(logger.info(s"开始验证教师 token: ${teacherToken}"))
      maybeTeacherID <- validateTeacherToken(teacherToken)

      teacherID <- maybeTeacherID match {
        case Some(id) => IO.pure(id)
        case None =>
          val errorMessage = s"教师 token 验证失败: ${teacherToken}"
          IO(logger.error(errorMessage)) >> IO.raiseError(new Exception(errorMessage))
      }

      _ <- IO(logger.info(s"教师 token 验证通过，教师 ID 为: ${teacherID}"))

      // Step 2: Query courses taught by the teacher
      ownCourses <- queryTeacherCourses(teacherID)

      _ <- IO(logger.info(s"查询到教师开设的课程记录共 ${ownCourses.length} 条"))
    } yield ownCourses
  }

  private def queryTeacherCourses(teacherID: Int)(using PlanContext): IO[List[CourseInfo]] = {
    for {
      _ <- IO(logger.info("开始创建获取教师课程的数据库查询指令"))
      querySql <- IO.pure(
        s"""
           SELECT course_id, course_capacity, time, location, course_group_id, teacher_id
           FROM ${schemaName}.course_table
           WHERE teacher_id = ?;
         """
      )
      _ <- IO(logger.info(s"数据库查询指令为: ${querySql}"))
      parameters <- IO.pure(List(SqlParameter("Int", teacherID.toString)))

      _ <- IO(logger.info("开始执行数据库查询指令"))
      rows <- readDBRows(querySql, parameters)

      _ <- IO(logger.info(s"从数据库查询结果中解析课程信息，结果包含 ${rows.length} 条记录"))
      courses <- IO {
        rows.map { json =>
          val courseID = decodeField[Int](json, "course_id")
          val courseCapacity = decodeField[Int](json, "course_capacity")
          val timeJson = decodeField[String](json, "time")
          val timeList = decodeType[List[CourseTime]](timeJson)
          val location = decodeField[String](json, "location")
          val courseGroupID = decodeField[Int](json, "course_group_id")
          val teacherID = decodeField[Int](json, "teacher_id")

          CourseInfo(
            courseID = courseID,
            courseCapacity = courseCapacity,
            time = timeList,
            location = location,
            courseGroupID = courseGroupID,
            teacherID = teacherID,
            preselectedStudentsSize = 0, // 初始化值，后续可以补充数值
            selectedStudentsSize = 0,   // 初始化值，后续可以补充数值
            waitingListSize = 0         // 初始化值，后续可以补充数值
          )
        }
      }
    } yield courses
  }
}