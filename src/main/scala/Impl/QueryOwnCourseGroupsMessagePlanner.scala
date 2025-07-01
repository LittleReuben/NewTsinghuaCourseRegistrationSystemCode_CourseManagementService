package Impl


import Common.API.{PlanContext, Planner}
import Common.DBAPI._
import Common.Object.SqlParameter
import Common.ServiceUtils.schemaName
import Objects.CourseManagementService.CourseGroup
import Utils.CourseManagementProcess.validateTeacherToken
import cats.effect.IO
import org.slf4j.LoggerFactory
import io.circe._
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
import APIs.UserAuthService.VerifyTokenValidityMessage
import Objects.UserAccountService.UserRole
import Objects.UserAccountService.SafeUserInfo
import Utils.CourseManagementProcess.validateTeacherToken
import Common.Serialize.CustomColumnTypes.{decodeDateTime,encodeDateTime}

case class QueryOwnCourseGroupsMessagePlanner(
                                               teacherToken: String,
                                               override val planContext: PlanContext
                                             ) extends Planner[List[CourseGroup]] {

  val logger = LoggerFactory.getLogger(this.getClass.getSimpleName + "_" + planContext.traceID.id)

  override def plan(using PlanContext): IO[List[CourseGroup]] = {
    for {
      // Step 1: Validate the teacher token and retrieve teacherID
      _ <- IO(logger.info(s"[Step 1] 验证教师 token: ${teacherToken}"))
      maybeTeacherID <- validateTeacherToken(teacherToken)

      teacherID <- maybeTeacherID match {
        case None =>
          IO(logger.error("[Step 1.1] 教师鉴权失败，无法继续操作，返回错误")) >>
            IO.raiseError(new IllegalArgumentException("教师鉴权失败"))
        case Some(id) =>
          IO(logger.info(s"[Step 1.2] 教师 token 验证成功，teacherID 是: ${id}")) >>
            IO.pure(id)
      }

      // Step 2: Query CourseGroupTable to retrieve all course groups created by teacherID
      _ <- IO(logger.info(s"[Step 2] 开始查询由教师创建的课程组，teacherID: ${teacherID}"))
      courseGroups <- queryOwnCourseGroups(teacherID)

      _ <- IO(logger.info(s"[Step 3] 查询完成，共找到 ${courseGroups.length} 个课程组"))
    } yield courseGroups
  }

  private def queryOwnCourseGroups(teacherID: Int)(using PlanContext): IO[List[CourseGroup]] = {
    val sql =
      s"""
         |SELECT course_group_id, name, credit, owner_teacher_id
         |FROM ${schemaName}.course_group_table
         |WHERE owner_teacher_id = ?;
       """.stripMargin

    for {
      _ <- IO(logger.info(s"[queryOwnCourseGroups] 执行数据库查询指令: $sql"))
      rows <- readDBRows(sql, List(SqlParameter("Int", teacherID.toString)))

      courseGroups <- IO {
        rows.map { row =>
          CourseGroup(
            courseGroupID = decodeField[Int](row, "course_group_id"),
            name = decodeField[String](row, "name"),
            credit = decodeField[Int](row, "credit"),
            ownerTeacherID = decodeField[Int](row, "owner_teacher_id"),
            authorizedTeachers = List.empty // Assuming this is not retrieved from CourseGroupTable
          )
        }
      }

      _ <- IO(logger.info(s"[queryOwnCourseGroups] 完成从数据库转换为 CourseGroup 对象，共转换 ${courseGroups.length} 个课程组"))
    } yield courseGroups
  }
}