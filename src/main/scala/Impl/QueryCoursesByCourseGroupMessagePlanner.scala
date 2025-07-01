package Impl


import Objects.CourseManagementService.{CourseTime, CourseGroup, DayOfWeek, TimePeriod, CourseInfo}
import Utils.CourseManagementProcess.fetchCourseGroupByID
import Common.API.{PlanContext, Planner}
import Common.DBAPI._
import Common.Object.SqlParameter
import Common.ServiceUtils.schemaName
import cats.effect.IO
import org.slf4j.LoggerFactory
import io.circe._
import io.circe.syntax._
import io.circe.generic.auto._
import org.joda.time.DateTime
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
import Objects.CourseManagementService.CourseGroup
import Objects.CourseManagementService.DayOfWeek
import Objects.CourseManagementService.TimePeriod
import Objects.CourseManagementService.CourseInfo
import cats.implicits._
import Common.Serialize.CustomColumnTypes.{decodeDateTime,encodeDateTime}
import Objects.CourseManagementService.CourseInfo

case class QueryCoursesByCourseGroupMessagePlanner(
  courseGroupID: Int,
  override val planContext: PlanContext
) extends Planner[List[CourseInfo]] {
  val logger = LoggerFactory.getLogger(this.getClass.getSimpleName + "_" + planContext.traceID.id)

  override def plan(using PlanContext): IO[List[CourseInfo]] = {
    for {
      // Step 1: Validate `courseGroupID` parameter.
      _ <- IO(logger.info(s"验证courseGroupID=${courseGroupID}的有效性"))
      courseGroupOpt <- validateCourseGroupID(courseGroupID)
      _ <- IO(logger.info(s"验证完成，课程组: ${courseGroupOpt.getOrElse("不存在")}"))

      // Step 2: Query course data.
      _ <- IO(logger.info(s"开始查询与课程组${courseGroupID}相关的课程数据"))
      courses <- queryCourses(courseGroupID)
      _ <- IO(logger.info(s"查询课程数据完成，共查询到${courses.size}条课程记录"))

    } yield courses
  }

  /**
   * Validate the courseGroupID by fetching related course group data.
   * If the course group does not exist, throw an error.
   */
  private def validateCourseGroupID(courseGroupID: Int)(using PlanContext): IO[Option[CourseGroup]] = {
    fetchCourseGroupByID(courseGroupID).flatMap {
      case None =>
        IO {
          val errorMessage = s"课程组[courseGroupID=${courseGroupID}]不存在，请确认输入参数是否正确"
          logger.error(errorMessage)
          throw new IllegalArgumentException(errorMessage)
        }
      case Some(courseGroup) =>
        IO.pure(Some(courseGroup))
    }
  }

  /**
   * Query all courses associated with the courseGroupID and map them to a list of CourseInfo instances.
   */
  private def queryCourses(courseGroupID: Int)(using PlanContext): IO[List[CourseInfo]] = {
    val sql =
      s"""
SELECT course_id, course_group_id, course_capacity, time, location, teacher_id
FROM ${schemaName}.course_table
WHERE course_group_id = ?
       """.stripMargin

    logger.info(s"执行SQL: $sql [参数: courseGroupID=$courseGroupID]")

    readDBRows(sql, List(SqlParameter("Int", courseGroupID.toString))).flatMap { rows =>
      IO {
        rows.map { row =>
          val courseID = decodeField[Int](row, "course_id")
          val courseCapacity = decodeField[Option[Int]](row, "course_capacity").getOrElse(0)
          val timeJson = decodeField[String](row, "time")
          val courseTime = decodeType[List[CourseTime]](timeJson)
          val location = decodeField[Option[String]](row, "location").getOrElse("")
          val teacherID = decodeField[Int](row, "teacher_id")
          val preselectedStudentsSize = 0 // 默认值：提前选课的学生数量不支持动态读取
          val selectedStudentsSize = 0   // 默认值：最终已选课程的学生数量
          val waitingListSize = 0        // 默认值：等待名单上的学生数量

          CourseInfo(
            courseID = courseID,
            courseCapacity = courseCapacity,
            time = courseTime,
            location = location,
            courseGroupID = courseGroupID,
            teacherID = teacherID,
            preselectedStudentsSize = preselectedStudentsSize,
            selectedStudentsSize = selectedStudentsSize,
            waitingListSize = waitingListSize
          )
        }
      }
    }
  }
}