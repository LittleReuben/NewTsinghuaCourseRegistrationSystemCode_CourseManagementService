package Impl


/**
 * Planner for QueryOwnCourseGroupsMessage: 用于处理查看课程组的功能需求
 */
import APIs.UserAuthService.VerifyTokenValidityMessage
import Objects.CourseManagementService.CourseGroup
import Objects.UserAccountService.UserRole
import Objects.UserAccountService.SafeUserInfo
import Utils.CourseManagementProcess.validateTeacherToken
import Common.API.{PlanContext, Planner}
import Common.DBAPI._
import Common.Object.SqlParameter
import Common.ServiceUtils.schemaName
import cats.effect.IO
import org.slf4j.LoggerFactory
import org.joda.time.DateTime
import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._
import cats.implicits._
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
import Utils.CourseManagementProcess.validateTeacherToken
import cats.implicits.*
import Common.Serialize.CustomColumnTypes.{decodeDateTime,encodeDateTime}

case class QueryOwnCourseGroupsMessagePlanner(
                                               teacherToken: String,
                                               override val planContext: PlanContext
                                             ) extends Planner[List[CourseGroup]] {
  private val logger = LoggerFactory.getLogger(this.getClass.getSimpleName + "_" + planContext.traceID.id)

  /**
   * Plan Method (核心实现逻辑)
   */
  override def plan(using planContext: PlanContext): IO[List[CourseGroup]] = {
    for {
      // Step 1: 验证教师 token 并获取教师 ID
      teacherID <- validateTokenAndFetchTeacherID(teacherToken)

      // Step 2: 查询教师 ID 创建的课程组信息
      _ <- IO(logger.info(s"开始查询课程组信息，教师ID为: ${teacherID}"))
      courseGroups <- queryCourseGroupsByTeacherID(teacherID)

      _ <- IO(logger.info(s"查询到课程组列表，数量为: ${courseGroups.size}"))
    } yield courseGroups
  }

  /**
   * 验证教师 token 并获取教师 ID
   * @param teacherToken 教师 token
   * @return 教师 ID
   */
  private def validateTokenAndFetchTeacherID(teacherToken: String)(using PlanContext): IO[Int] = {
    for {
      _ <- IO(logger.info(s"开始验证教师 token: ${teacherToken}"))
      teacherIDOpt <- validateTeacherToken(teacherToken)

      // 鉴权失败处理
      _ <- teacherIDOpt match {
        case None => IO(logger.error(s"教师 token 验证失败: ${teacherToken}"))
        case Some(_) => IO.unit
      }

      teacherID <- teacherIDOpt match {
        case None => IO.raiseError(new IllegalStateException("教师鉴权失败。"))
        case Some(id) => IO.pure(id)
      }
    } yield teacherID
  }

  /**
   * 查询课程组信息
   * @param teacherID 教师 ID
   * @return List[CourseGroup]
   */
  private def queryCourseGroupsByTeacherID(teacherID: Int)(using PlanContext): IO[List[CourseGroup]] = {
    val sql =
      s"""
         |SELECT course_group_id, name, credit, owner_teacher_id
         |FROM ${schemaName}.course_group_table
         |WHERE owner_teacher_id = ?;
      """.stripMargin

    for {
      _ <- IO(logger.info(s"执行查询课程组信息的 SQL 指令: ${sql}"))
      rowsJson <- readDBRows(
        sql,
        List(SqlParameter("Int", teacherID.toString))
      )
      courseGroups <- IO(rowsJson.map(decodeType[CourseGroup]))
    } yield courseGroups
  }
}