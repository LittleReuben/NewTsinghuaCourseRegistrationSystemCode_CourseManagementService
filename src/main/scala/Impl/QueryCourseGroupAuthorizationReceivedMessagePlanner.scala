package Impl



import APIs.UserAuthService.VerifyTokenValidityMessage
import Utils.CourseManagementProcess.fetchCourseGroupByID
import Objects.CourseManagementService.CourseGroup
import Utils.CourseManagementProcess.validateTeacherToken
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
import Common.DBAPI._
import Common.API.{PlanContext, Planner}
import cats.effect.IO
import Common.Object.SqlParameter
import Common.Serialize.CustomColumnTypes.{decodeDateTime,encodeDateTime}
import Common.ServiceUtils.schemaName
import Objects.UserAccountService.UserRole
import Objects.UserAccountService.SafeUserInfo
import Utils.CourseManagementProcess.validateTeacherToken
import org.joda.time.DateTime
import io.circe._
import io.circe.syntax._
import io.circe.generic.auto._
import cats.implicits.*
import Common.Serialize.CustomColumnTypes.{decodeDateTime,encodeDateTime}

case class QueryCourseGroupAuthorizationReceivedMessagePlanner(
    teacherToken: String,
    override val planContext: PlanContext
) extends Planner[List[CourseGroup]] {
  val logger = LoggerFactory.getLogger(this.getClass.getSimpleName + "_" + planContext.traceID.id)

  override def plan(using planContext: PlanContext): IO[List[CourseGroup]] = {
    for {
      // Step 1: 验证Token的有效性并获取 teacherID
      _ <- IO(logger.info(s"[Step 1] 验证教师Token的有效性: token=${teacherToken}"))
      teacherIDOpt <- validateTeacherToken(teacherToken)

      teacherID <- teacherIDOpt match {
        case Some(id) => IO.pure(id)
        case None =>
          val errorMessage = s"教师Token验证失败: token=${teacherToken}"
          IO(logger.error(errorMessage)) >> IO.raiseError(new IllegalArgumentException(errorMessage))
      }
      _ <- IO(logger.info(s"[Step 1.1] 教师Token验证通过, teacherID=${teacherID}"))

      // Step 2: 查询教师被授权的课程组ID列表
      authorizedCourseGroupIDs <- getAuthorizedCourseGroupIDs(teacherID)
      _ <- IO(logger.info(s"[Step 2] 教师被授权的课程组ID列表: ${authorizedCourseGroupIDs.mkString(", ")}"))

      // Step 3: 查询课程组详细信息
      courseGroupDetails <- fetchCourseGroupDetails(authorizedCourseGroupIDs)
      _ <- IO(logger.info(s"[Step 3] 已获取课程组详情，总数=${courseGroupDetails.size}"))

    } yield courseGroupDetails
  }

  // Step 2.1: 在AuthorizedTeachersTable中查询teacherID对应的所有课程组ID
  private def getAuthorizedCourseGroupIDs(teacherID: Int)(using PlanContext): IO[List[Int]] = {
    val sql = s"""
      SELECT course_group_id 
      FROM ${schemaName}.authorized_teachers_table 
      WHERE authorized_teacher_id = ?
    """
    for {
      _ <- IO(logger.info(s"执行SQL: ${sql}, 查询授权的课程组ID"))
      resultRows <- readDBRows(sql, List(SqlParameter("Int", teacherID.toString)))
      courseGroupIDs <- IO(resultRows.map(json => decodeField[Int](json, "course_group_id")))
      _ <- IO(logger.info(s"查询结果，课程组IDs: ${courseGroupIDs.mkString(", ")}"))
    } yield courseGroupIDs
  }

  // Step 3.1: 根据课程组ID列表查询课程组详细信息
  private def fetchCourseGroupDetails(courseGroupIDs: List[Int])(using PlanContext): IO[List[CourseGroup]] = {
    val logger = LoggerFactory.getLogger("fetchCourseGroupDetails")
    val courseGroupDetailsIOs = courseGroupIDs.map { courseGroupID =>
      fetchCourseGroupByID(courseGroupID).flatMap {
        case Some(courseGroup) =>
          IO(logger.info(s"获取到课程组详情: ${courseGroup}")) >> IO.pure(Some(courseGroup))
        case None =>
          IO(logger.warn(s"未找到课程组ID: ${courseGroupID} 的详情")) >> IO.pure(None)
      }
    }
    courseGroupDetailsIOs.sequence.map(_.flatten)
  }
}