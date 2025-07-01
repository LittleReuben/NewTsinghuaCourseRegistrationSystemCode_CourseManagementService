package Impl


/**
 * Planner for QueryCourseGroupByIDMessage: 根据课程组ID查询课程组信息
 *
 * @param userToken 用户的鉴权Token
 * @param courseGroupID 课程组ID
 * @param planContext 计划执行上下文
 */
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
import Common.Object.SqlParameter
import Common.ServiceUtils.schemaName
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
import Objects.CourseManagementService.CourseGroup
import Common.Serialize.CustomColumnTypes.{decodeDateTime,encodeDateTime}

case class QueryCourseGroupByIDMessagePlanner(userToken: String, courseGroupID: Int, override val planContext: PlanContext) extends Planner[CourseGroup] {
  val logger = LoggerFactory.getLogger(this.getClass.getSimpleName + "_" + planContext.traceID.id)

  override def plan(using planContext: PlanContext): IO[CourseGroup] = {

    // Step 1: 验证用户的Token有效性
    def validateToken(): IO[Unit] = for {
      _ <- IO(logger.info(s"[Step 1] 开始验证用户Token的有效性：userToken=${userToken}"))
      isTokenValid <- VerifyTokenValidityMessage(userToken).send
      _ <- if (isTokenValid) {
        IO(logger.info("[Step 1.1] 用户Token验证成功"))
      } else {
        IO(logger.error("[Step 1.1] 用户Token验证失败")) >>
          IO.raiseError(new IllegalArgumentException("Invalid user token"))
      }
    } yield ()

    // Step 2: 根据课程组ID查询课程组信息
    def getCourseGroup(): IO[CourseGroup] = for {
      _ <- IO(logger.info(s"[Step 2] 查询课程组信息：courseGroupID=${courseGroupID}"))
      courseGroupOpt <- fetchCourseGroupByID(courseGroupID)
      _ <- IO(logger.info(s"[Step 2.1] 课程组信息查询完成：${courseGroupOpt.map(_.toString).getOrElse("未找到课程组信息")}"))
      courseGroup <- IO.fromOption(courseGroupOpt)(
        new NoSuchElementException(s"[QueryCourseGroupByID] 对应课程组ID=${courseGroupID}的信息不存在")
      )
      _ <- IO(logger.info(s"[Step 2.2] 课程组已找到：" +
        s"courseGroupID=${courseGroup.courseGroupID}, " +
        s"name=${courseGroup.name}, credit=${courseGroup.credit}, " +
        s"ownerTeacherID=${courseGroup.ownerTeacherID}, " +
        s"authorizedTeachers=${courseGroup.authorizedTeachers.mkString("[", ", ", "]")}"))
    } yield courseGroup

    // Orchestration: 执行Plan步骤
    for {
      _ <- validateToken()  // 验证Token步骤
      courseGroup <- getCourseGroup()  // 查询课程组步骤
    } yield courseGroup
  }
}