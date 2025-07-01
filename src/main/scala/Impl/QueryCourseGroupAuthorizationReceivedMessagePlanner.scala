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
import cats.implicits._
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
import io.circe.syntax._
import io.circe.generic.auto._
import org.joda.time.DateTime
import cats.implicits.*
import Common.Serialize.CustomColumnTypes.{decodeDateTime,encodeDateTime}

case class QueryCourseGroupAuthorizationReceivedMessagePlanner(
    teacherToken: String,
    override val planContext: PlanContext
) extends Planner[List[CourseGroup]] {

  val logger = LoggerFactory.getLogger(this.getClass.getSimpleName + "_" + planContext.traceID.id)

  override def plan(using PlanContext): IO[List[CourseGroup]] = {
    for {
      // Step 1: 验证教师 Token 的有效性并获取 teacherID
      _ <- IO(logger.info(s"验证教师 Token 的有效性: $teacherToken"))
      teacherIDOpt <- validateTeacherToken(teacherToken)
      teacherID <- teacherIDOpt match {
        case None =>
          IO.raiseError(new IllegalStateException("教师鉴权失败"))
        case Some(id) =>
          IO(logger.info(s"教师鉴权成功，教师 ID 为 $id")).as(id)
      }

      // Step 2: 查询教师被授权的课程组 ID 列表
      _ <- IO(logger.info(s"查询教师被授权的课程组 ID 列表, 教师ID=$teacherID"))
      courseGroupIDs <- queryAuthorizedCourseGroupIDs(teacherID)

      // Step 3: 使用课程组 ID 查询课程组详细信息
      _ <- IO(logger.info(s"根据课程组 ID 列表查询课程组详细信息, 被授权的课程组列表IDs=${courseGroupIDs.mkString(",")}"))
      courseGroups <- queryCourseGroupsByIDs(courseGroupIDs)

    } yield courseGroups
  }

  // 子步骤：根据教师 ID 查询授权的课程组 ID 列表
  private def queryAuthorizedCourseGroupIDs(teacherID: Int)(using PlanContext): IO[List[Int]] = {
    val query = s"""
      SELECT course_group_id
      FROM ${schemaName}.authorized_teachers_table
      WHERE authorized_teacher_id = ?
    """
    for {
      _ <- IO(logger.info(s"执行 SQL: $query，查询教师ID=$teacherID的被授权课程组 IDs"))
      rows <- readDBRows(query, List(SqlParameter("Int", teacherID.toString)))
    } yield rows.map(row => decodeField[Int](row, "course_group_id"))
  }

  // 子步骤：根据课程组 ID 列表查询课程组详细信息
  private def queryCourseGroupsByIDs(courseGroupIDs: List[Int])(using PlanContext): IO[List[CourseGroup]] = {
    courseGroupIDs.traverse { id =>
      fetchCourseGroupByID(id).flatMap {
        case None =>
          IO(logger.warn(s"课程组 ID=$id 不存在，跳过")).as(None)
        case Some(courseGroup) =>
          IO(logger.info(s"课程组 ID=$id 查询成功: $courseGroup")).as(Some(courseGroup))
      }
    }.map(_.flatten)
  }
}