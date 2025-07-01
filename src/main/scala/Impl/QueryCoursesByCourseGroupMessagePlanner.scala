package Impl


/**
 * Planner for QueryCoursesByCourseGroup:
 * 查询课程组特定课程信息
 */
import Common.API.{PlanContext, Planner}
import Common.DBAPI._
import Common.Object.SqlParameter
import Common.ServiceUtils.schemaName
import Utils.CourseManagementProcess.fetchCourseGroupByID
import Objects.CourseManagementService.CourseInfo
import Objects.CourseManagementService.CourseTime
import Objects.CourseManagementService.DayOfWeek
import Objects.CourseManagementService.TimePeriod
import Objects.CourseManagementService.CourseGroup
import org.slf4j.LoggerFactory
import cats.effect.IO
import io.circe.Json
import io.circe.parser.decode
import io.circe.syntax._
import io.circe.generic.auto._
import org.joda.time.DateTime
import cats.implicits.*
import Common.Serialize.CustomColumnTypes.{decodeDateTime, encodeDateTime}
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
import cats.implicits._
import io.circe._
import Common.Serialize.CustomColumnTypes.{decodeDateTime,encodeDateTime}

case class QueryCoursesByCourseGroupMessagePlanner(
                                                    courseGroupID: Int,
                                                    override val planContext: PlanContext
                                                  ) extends Planner[List[CourseInfo]] {

  val logger = LoggerFactory.getLogger(this.getClass.getSimpleName + "_" + planContext.traceID.id)

  override def plan(using planContext: PlanContext): IO[List[CourseInfo]] = {
    for {
      // Step 1: 验证 CourseGroupID 的合法性
      _ <- IO(logger.info(s"[Step 1] 验证 CourseGroupID=${courseGroupID} 的合法性"))
      courseGroup <- validateCourseGroupID(courseGroupID)
      _ <- IO(logger.info(s"[验证完成] CourseGroupID合法: ${courseGroup}"))

      // Step 2: 查询课程数据
      _ <- IO(logger.info(s"[Step 2] 查询课程数据，CourseGroupID=${courseGroupID}"))
      courses <- queryCourses(courseGroupID)
      _ <- IO(logger.info(s"[查询完成] 返回课程信息封装，数量=${courses.size}"))

    } yield courses
  }

  private def validateCourseGroupID(courseGroupID: Int)(using PlanContext): IO[CourseGroup] = {
    logger.info(s"调用 fetchCourseGroupByID 方法检查课程组是否存在, CourseGroupID=${courseGroupID}")
    fetchCourseGroupByID(courseGroupID).flatMap {
      case None =>
        val errorMessage = s"课程组ID ${courseGroupID} 不存在，无法查询课程信息"
        IO(logger.error(errorMessage)) >>
          IO.raiseError(new IllegalArgumentException(errorMessage))
      case Some(courseGroup) =>
        IO(logger.info(s"课程组验证通过，可以访问: ${courseGroup}")).map(_ => courseGroup)
    }
  }

  private def queryCourses(courseGroupID: Int)(using PlanContext): IO[List[CourseInfo]] = {
    val sql =
      s"""
       SELECT course_id, course_capacity, time, location, teacher_id
       FROM ${schemaName}.course_table
       WHERE course_group_id = ?;
       """.stripMargin

    for {
      _ <- IO(logger.info(s"[QueryCourses] 执行查询SQL：${sql}"))
      rows <- readDBRows(sql, List(SqlParameter("Int", courseGroupID.toString)))
      _ <- IO(logger.info(s"[QueryCourses] 数据库查询完成，共返回 ${rows.size} 行，准备封装"))

      // Traverse 封装课程信息
      courses <- rows.traverse { row =>
        IO {
          val courseID = decodeField[Int](row, "course_id")
          val courseCapacity = decodeField[Option[Int]](row, "course_capacity").getOrElse(0)
          val timeJsonString = decodeField[String](row, "time")
          val location = decodeField[String](row, "location")
          val teacherID = decodeField[Int](row, "teacher_id")

          // 封装时间信息
          val time = decode[List[Json]](timeJsonString).fold(
            ex => {
              logger.error(s"[课程时间解析失败] 错误信息：${ex.getMessage}, 原始时间JSON：${timeJsonString}")
              throw ex
            },
            _.map { timeJson =>
              val dayOfWeek = DayOfWeek.fromString(decodeField[String](timeJson, "dayOfWeek"))
              val timePeriod = TimePeriod.fromString(decodeField[String](timeJson, "timePeriod"))
              CourseTime(dayOfWeek, timePeriod)
            }
          )

          // 封装 CourseInfo 对象
          CourseInfo(
            courseID = courseID,
            courseCapacity = courseCapacity,
            time = time,
            location = location,
            courseGroupID = courseGroupID,
            teacherID = teacherID,
            preselectedStudentsSize = 0, // 不涉及预选
            selectedStudentsSize = 0, // 不涉及最终选择
            waitingListSize = 0 // 不涉及等待名单
          )
        }
      }

      _ <- IO(logger.info(s"[封装完成] CourseInfo 已完成转换，数量=${courses.size}"))
    } yield courses
  }
}