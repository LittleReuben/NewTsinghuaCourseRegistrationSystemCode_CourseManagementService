package Impl


import APIs.UserAuthService.VerifyTokenValidityMessage
import Objects.UserAccountService.UserRole
import Objects.UserAccountService.SafeUserInfo
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
import Utils.CourseManagementProcess.validateTeacherToken

case class QueryCourseGroupAuthorizedTeachersMessagePlanner(
                                                             teacherToken: String,
                                                             courseGroupID: Int,
                                                             override val planContext: PlanContext
                                                           ) extends Planner[List[Int]] {
  val logger = LoggerFactory.getLogger(this.getClass.getSimpleName + "_" + planContext.traceID.id)

  override def plan(using planContext: PlanContext): IO[List[Int]] = {
    for {
      // Step 1: 验证 teacherToken 并获取 teacherID
      _ <- IO(logger.info(s"开始验证教师 token: ${teacherToken}"))
      teacherIDOpt <- validateTeacherToken(teacherToken)
      teacherID <- teacherIDOpt match {
        case None => IO.raiseError(new IllegalStateException("教师鉴权失败。"))
        case Some(value) =>
          IO(logger.info(s"教师 token 验证成功, ID: ${value}")) >> IO.pure(value)
      }

      // Step 2: 验证课程组是否存在及权限检查
      _ <- IO(logger.info(s"验证课程组信息以及权限, courseGroupID: ${courseGroupID}, teacherID: ${teacherID}"))
      courseGroupJsonOpt <- checkCourseGroupOwnership(courseGroupID, teacherID)
      courseGroupJson <- courseGroupJsonOpt match {
        case None => IO.raiseError(new IllegalStateException("无权操作该课程组。"))
        case Some(courseGroupJson) =>
          IO(logger.info(s"课程组存在且权限验证通过: ${courseGroupJson}")) >>
            IO.pure(courseGroupJson)
      }

      // Step 3: 查询与课程组相关的授权教师
      _ <- IO(logger.info(s"查询课程组 ${courseGroupID} 的授权教师信息"))
      authorizedTeacherIDs <- queryAuthorizedTeachers(courseGroupID)

      _ <- IO(logger.info(s"授权教师查询完成，共 ${authorizedTeacherIDs.size} 个授权教师"))
    } yield authorizedTeacherIDs
  }

  private def checkCourseGroupOwnership(courseGroupID: Int, teacherID: Int)(using PlanContext): IO[Option[Json]] = {
    logger.info(s"开始验证课程组是否存在及权限，courseGroupID: ${courseGroupID}, teacherID: ${teacherID}")
    val sql =
      s"""
        SELECT *
        FROM ${schemaName}.course_group_table
        WHERE course_group_id = ? AND owner_teacher_id = ?;
       """
    readDBJsonOptional(
      sql,
      List(
        SqlParameter("Int", courseGroupID.toString),
        SqlParameter("Int", teacherID.toString)
      )
    )
  }

  private def queryAuthorizedTeachers(courseGroupID: Int)(using PlanContext): IO[List[Int]] = {
    logger.info(s"开始查询课程组相关的授权教师信息，courseGroupID: ${courseGroupID}")
    val sql =
      s"""
        SELECT authorized_teacher_id
        FROM ${schemaName}.authorized_teachers_table
        WHERE course_group_id = ?;
       """
    readDBRows(sql, List(SqlParameter("Int", courseGroupID.toString))).map {
      _.map(decodeField[Int](_, "authorized_teacher_id"))
    }
  }
}