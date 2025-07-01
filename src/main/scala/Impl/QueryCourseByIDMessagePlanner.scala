package Impl


import APIs.UserAuthService.VerifyTokenValidityMessage
import Utils.CourseManagementProcess.fetchCourseByID
import Objects.CourseManagementService.CourseTime
import Objects.CourseManagementService.DayOfWeek
import Objects.CourseManagementService.TimePeriod
import Objects.CourseManagementService.CourseInfo
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
import org.joda.time.DateTime
import cats.implicits.*
import Common.DBAPI._
import Common.API.{PlanContext, Planner}
import cats.effect.IO
import Common.Object.SqlParameter
import Common.Serialize.CustomColumnTypes.{decodeDateTime,encodeDateTime}
import Common.ServiceUtils.schemaName
import Objects.CourseManagementService.CourseInfo
import io.circe._
import io.circe.syntax._
import io.circe.generic.auto._
import cats.implicits.*
import Common.Serialize.CustomColumnTypes.{decodeDateTime,encodeDateTime}

case class QueryCourseByIDMessagePlanner(
   userToken: String,
   courseID: Int,
   override val planContext: PlanContext
) extends Planner[CourseInfo] {

  val logger = LoggerFactory.getLogger(this.getClass.getSimpleName + "_" + planContext.traceID.id)

  override def plan(using context: PlanContext): IO[CourseInfo] = {
    for {
      // Step 1: Verify token validity
      _ <- IO(logger.info(s"Step 1: 验证用户令牌有效性: ${userToken}"))
      isTokenValid <- verifyToken(userToken)
      _ <- if (!isTokenValid) {
        IO.raiseError(new IllegalStateException("用户令牌无效或过期"))
      } else {
        IO(logger.info(s"令牌有效"))
      }

      // Step 2: Query course information by course ID
      _ <- IO(logger.info(s"Step 2: 根据课程ID ${courseID} 查询课程信息"))
      courseInfo <- queryCourseByID(courseID)
    } yield courseInfo
  }

  private def verifyToken(userToken: String)(using PlanContext): IO[Boolean] = {
    logger.info(s"调用 VerifyTokenValidityMessage 接口")
    VerifyTokenValidityMessage(userToken).send.flatMap { result =>
      logger.info(s"令牌验证结果: ${result}")
      IO(result)
    }
  }

  private def queryCourseByID(courseID: Int)(using PlanContext): IO[CourseInfo] = {
    fetchCourseByID(courseID).flatMap {
      case Some(courseInfo) =>
        IO(logger.info(s"课程信息查询成功: ${courseInfo}")) >>
        IO(courseInfo)
      case None =>
        val errorMessage = s"课程ID ${courseID} 对应的课程信息不存在"
        logger.error(errorMessage)
        IO.raiseError(new IllegalStateException(errorMessage))
    }
  }
}