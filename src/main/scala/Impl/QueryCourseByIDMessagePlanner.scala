package Impl


import APIs.UserAuthService.VerifyTokenValidityMessage
import Utils.CourseManagementProcess.fetchCourseByID
import Objects.CourseManagementService.{CourseInfo, CourseTime, DayOfWeek, TimePeriod}
import Common.API.{PlanContext, Planner}
import Common.Object.SqlParameter
import cats.effect.IO
import org.slf4j.LoggerFactory
import io.circe._
import io.circe.syntax._
import io.circe.generic.auto._
import org.joda.time.DateTime
import cats.implicits.*
import Common.DBAPI._
import Common.Serialize.CustomColumnTypes.{decodeDateTime, encodeDateTime}
import Common.ServiceUtils.schemaName
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
import Common.Serialize.CustomColumnTypes.{decodeDateTime,encodeDateTime}
import Objects.CourseManagementService.CourseInfo

case class QueryCourseByIDMessagePlanner(
    userToken: String,
    courseID: Int,
    override val planContext: PlanContext
) extends Planner[CourseInfo] {
  val logger = LoggerFactory.getLogger(this.getClass.getSimpleName + "_" + planContext.traceID.id)

  override def plan(using planContext: PlanContext): IO[CourseInfo] = {
    for {
      // Step 1: Validate the user token
      _ <- IO(logger.info(s"验证用户令牌: userToken=${userToken}"))
      isValid <- validateUserToken(userToken)
      _ <- IO(logger.info(s"用户令牌验证结果: ${isValid}"))

      // If invalid, throw an error
      _ <- if (!isValid) {
        IO.raiseError(new IllegalArgumentException("用户令牌无效或过期"))
      } else {
        IO.unit
      }

      // Step 2: Fetch course by ID
      _ <- IO(logger.info(s"根据课程ID查询课程信息: courseID=${courseID}"))
      courseOption <- fetchCourseInfo(courseID)
      _ <- IO(logger.info(s"查询课程信息结果是否存在: ${courseOption.isDefined}"))

      // If course does not exist, throw an error
      course <- courseOption match {
        case Some(course) => IO(course)
        case None => IO.raiseError(new NoSuchElementException("课程不存在"))
      }
    } yield course
  }

  // Step 1: Validate token using external API
  private def validateUserToken(userToken: String)(using PlanContext): IO[Boolean] = {
    VerifyTokenValidityMessage(userToken).send.map { isValid =>
      logger.info(s"用户令牌 ${userToken} 验证结果: ${isValid}")
      isValid
    }
  }

  // Step 2: Fetch course information using utility method
  private def fetchCourseInfo(courseID: Int)(using PlanContext): IO[Option[CourseInfo]] = {
    fetchCourseByID(courseID).map { courseOption =>
      courseOption.foreach { course =>
        logger.info(s"查询到的课程信息: courseID=${course.courseID}, location=${course.location}")
      }
      courseOption
    }
  }
}