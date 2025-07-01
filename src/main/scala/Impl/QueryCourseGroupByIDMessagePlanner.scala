package Impl


import APIs.UserAuthService.VerifyTokenValidityMessage
import Utils.CourseManagementProcess.fetchCourseGroupByID
import Objects.CourseManagementService.CourseGroup
import Common.API.{PlanContext, Planner}
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
import Objects.CourseManagementService.CourseGroup
import io.circe._
import io.circe.syntax._
import io.circe.generic.auto._
import org.joda.time.DateTime
import cats.implicits.*
import Common.DBAPI._
import Common.Object.SqlParameter
import Common.Serialize.CustomColumnTypes.{decodeDateTime,encodeDateTime}
import Common.ServiceUtils.schemaName

case class QueryCourseGroupByIDMessagePlanner(
    userToken: String,
    courseGroupID: Int,
    override val planContext: PlanContext
) extends Planner[CourseGroup] {

  val logger = LoggerFactory.getLogger(this.getClass.getSimpleName + "_" + planContext.traceID.id)

  override def plan(using planContext: PlanContext): IO[CourseGroup] = {
    for {
      // Step 1: Verify token validity
      _ <- IO(logger.info(s"[Step 1] 验证用户Token的有效性: userToken=${userToken}"))
      isValid <- VerifyTokenValidityMessage(userToken).send
      _ <- if (!isValid) {
        val errorMessage = s"[Step 1.1] 无效的用户Token: userToken=${userToken}"
        IO(logger.error(errorMessage)) >>
          IO.raiseError(new IllegalStateException(errorMessage))
      } else IO(logger.info("[Step 1.2] 用户Token验证通过"))

      // Step 2: Fetch course group information by ID
      _ <- IO(logger.info(s"[Step 2] 根据课程组ID查询课程组信息: courseGroupID=${courseGroupID}"))
      courseGroupOpt <- fetchCourseGroupByID(courseGroupID)
      _ <- courseGroupOpt match {
        case None =>
          val errorMessage = s"[Step 2.1] 课程组ID不存在: courseGroupID=${courseGroupID}"
          IO(logger.error(errorMessage)) >>
            IO.raiseError(new IllegalStateException(errorMessage))
        case Some(group) =>
          IO(logger.info(s"[Step 2.2] 成功获取课程组信息: ${group}"))
      }

      // Step 3: Unwrap the result to obtain the course group
      courseGroup <- IO(courseGroupOpt.get)
    } yield courseGroup
  }
}