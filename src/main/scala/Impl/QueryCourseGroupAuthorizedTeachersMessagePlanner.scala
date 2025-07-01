package Impl


import APIs.UserAuthService.VerifyTokenValidityMessage
import Common.API.{PlanContext, Planner}
import Common.DBAPI._
import Common.Object.SqlParameter
import Common.ServiceUtils.schemaName
import Utils.CourseManagementProcess.validateTeacherToken
import cats.effect.IO
import org.slf4j.LoggerFactory
import io.circe._
import io.circe.syntax._
import io.circe.generic.auto._
import org.joda.time.DateTime
import cats.implicits.*
import Common.Serialize.CustomColumnTypes.{decodeDateTime,encodeDateTime}
import Objects.UserAccountService.UserRole
import Objects.UserAccountService.SafeUserInfo
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

  override def plan(using PlanContext): IO[List[Int]] = for {
    // Step 1: Validate the teacher token and get teacherID
    teacherIDOpt <- validateTeacherToken(teacherToken)
    _ <- IO(logger.info(s"Token 验证结果: ${teacherIDOpt}"))
    teacherID <- teacherIDOpt match {
      case None =>
        IO.raiseError(new IllegalStateException("教师鉴权失败。"))
      case Some(id) =>
        IO.pure(id)
    }

    // Step 2: Verify the courseGroup exists and check ownership
    courseGroupOpt <- validateCourseGroupOwnership(teacherID, courseGroupID)
    _ <- IO(logger.info(s"${if (courseGroupOpt.isDefined) "课程组验证成功" else "课程组验证失败"}"))
    _ <- if (courseGroupOpt.isEmpty) IO.raiseError(new IllegalStateException("无权操作该课程组。")) else IO.unit

    // Step 3: Query authorized teachers in the course group
    authorizedTeachers <- queryAuthorizedTeachers(courseGroupID)
    _ <- IO(logger.info(s"查询到的授权教师ID列表: ${authorizedTeachers}"))

  } yield authorizedTeachers

  private def validateCourseGroupOwnership(teacherID: Int, courseGroupID: Int)(using PlanContext): IO[Option[Json]] = {
    logger.info("开始验证课程组拥有权与存在性")
    val sql =
      s"""
         SELECT * 
         FROM ${schemaName}.course_group_table 
         WHERE course_group_id = ? AND owner_teacher_id = ?
       """
    readDBJsonOptional(sql, List(
      SqlParameter("Int", courseGroupID.toString),
      SqlParameter("Int", teacherID.toString)
    ))
  }

  private def queryAuthorizedTeachers(courseGroupID: Int)(using PlanContext): IO[List[Int]] = {
    logger.info("开始查询课程组授权教师信息")
    val sql =
      s"""
         SELECT authorized_teacher_id 
         FROM ${schemaName}.authorized_teachers_table 
         WHERE course_group_id = ?
       """
    readDBRows(sql, List(SqlParameter("Int", courseGroupID.toString))).map { rows =>
      rows.map(row => decodeField[Int](row, "authorized_teacher_id"))
    }
  }
}