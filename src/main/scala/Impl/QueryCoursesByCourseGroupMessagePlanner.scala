package Impl

import Objects.CourseManagementService.CourseTime
import Utils.CourseManagementProcess.fetchCourseGroupByID
import Objects.CourseManagementService.CourseGroup
import Objects.CourseManagementService.DayOfWeek
import Objects.CourseManagementService.TimePeriod
import Objects.CourseManagementService.CourseInfo
import Common.API.{PlanContext, Planner}
import Common.DBAPI._
import Common.Object.SqlParameter
import Common.ServiceUtils.schemaName
import APIs.UserAuthService.VerifyTokenValidityMessage
import cats.effect.IO
import org.slf4j.LoggerFactory
import io.circe.Json
import cats.syntax.all._ // 修复 traverse 报错
import io.circe.parser.decode
import io.circe.generic.auto._

case class QueryCoursesByCourseGroupMessagePlanner(
                                                    userToken: String,
                                                    courseGroupID: Int,
                                                    override val planContext: PlanContext
                                                  ) extends Planner[List[CourseInfo]] {
  val logger = LoggerFactory.getLogger(this.getClass.getSimpleName + "_" + planContext.traceID.id)

  override def plan(using planContext: PlanContext): IO[List[CourseInfo]] = {
    for {
      // Step 1: 验证用户Token合法性
      _ <- IO(logger.info(s"[Step 1] 开始验证用户Token的有效性：userToken=${userToken}"))
      _ <- validateToken()
      
      // Step 2: 验证 CourseGroupID 的合法性
      _ <- IO(logger.info(s"[Step 2] 验证 CourseGroupID=${courseGroupID} 的合法性"))
      courseGroup <- validateCourseGroupID(courseGroupID)
      _ <- IO(logger.info(s"[验证完成] CourseGroupID合法: ${courseGroup}"))

      // Step 3: 查询课程数据
      _ <- IO(logger.info(s"[Step 3] 查询课程数据，CourseGroupID=${courseGroupID}"))
      courses <- fetchAllCourses(courseGroupID)
      _ <- IO(logger.info(s"[查询完成] 返回课程信息封装，数量=${courses.size}"))

    } yield courses
  }

  private def validateToken()(using PlanContext): IO[Unit] = for {
    _ <- IO(logger.info(s"[ValidateToken] 调用 VerifyTokenValidityMessage 验证用户Token：userToken=${userToken}"))
    isTokenValid <- VerifyTokenValidityMessage(userToken).send
    _ <- if (isTokenValid) {
      IO(logger.info("[ValidateToken] 用户Token验证成功"))
    } else {
      IO(logger.error("[ValidateToken] 用户Token验证失败")) >>
        IO.raiseError(new IllegalArgumentException("Invalid user token"))
    }
  } yield ()

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

  private def fetchAllCourses(courseGroupID: Int)(using PlanContext): IO[List[CourseInfo]] = {
    val sql =
      s"""
       SELECT course_id
       FROM ${schemaName}.course_table
       WHERE course_group_id = ?;
       """.stripMargin

    for {
      _ <- IO(logger.info(s"[FetchAllCourses] 执行查询SQL：${sql}"))
      rows <- readDBRows(sql, List(SqlParameter("Int", courseGroupID.toString)))
      _ <- IO(logger.info(s"[FetchAllCourses] 数据库查询完成，共返回 ${rows.size} 行，开始调用 fetchCourseByID"))

      courses <- rows.traverse { row =>
        val courseID = decodeField[Int](row, "course_id")
        Utils.CourseManagementProcess.fetchCourseByID(courseID).flatMap {
          case None =>
            val errorMessage = s"课程ID ${courseID} 不存在，无法封装课程信息"
            IO(logger.error(errorMessage)) >>
              IO.raiseError(new IllegalArgumentException(errorMessage))
          case Some(course) =>
            IO(logger.info(s"[FetchCourseByID] 课程信息封装成功: ${course}")).map(_ => course)
        }
      }

      _ <- IO(logger.info(s"[封装完成] CourseInfo 已完成转换，数量=${courses.size}"))
    } yield courses
  }
}