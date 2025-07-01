package Impl


import Common.API.{PlanContext, Planner}
import Common.DBAPI._
import Common.Object.SqlParameter
import Common.ServiceUtils.schemaName
import Utils.CourseManagementProcess.{validateTeacherToken, fetchCourseGroupByID, validateTeacherManagePermission, recordCourseGroupOperationLog}
import Objects.CourseManagementService.CourseGroup
import cats.effect.IO
import org.slf4j.LoggerFactory
import io.circe._
import io.circe.syntax._
import io.circe.generic.auto._
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
import Utils.CourseManagementProcess.recordCourseGroupOperationLog
import APIs.UserAuthService.VerifyTokenValidityMessage
import Utils.CourseManagementProcess.fetchCourseGroupByID
import Objects.SystemLogService.SystemLogEntry
import Objects.UserAccountService.UserRole
import Objects.UserAccountService.SafeUserInfo
import Utils.CourseManagementProcess.validateTeacherToken
import Utils.CourseManagementProcess.validateTeacherManagePermission
import org.joda.time.DateTime
import cats.implicits.*
import Common.Serialize.CustomColumnTypes.{decodeDateTime,encodeDateTime}
import Utils.CourseManagementProcess.validateTeacherManagePermission

case class GrantCourseGroupAuthorizationMessagePlanner(
  teacherToken: String,
  courseGroupID: Int,
  authorizedTeacherID: Int,
  override val planContext: PlanContext
) extends Planner[List[Int]] {
  val logger = LoggerFactory.getLogger(this.getClass.getSimpleName + "_" + planContext.traceID.id)

  override def plan(using planContext: PlanContext): IO[List[Int]] = {
    for {
      // Step 1: validate teacherToken and extract teacherID
      teacherID <- validateTeacherToken(teacherToken).flatMap {
        case Some(id) =>
          IO(logger.info(s"教师ID验证成功: teacherID=${id}")).as(id)
        case None =>
          IO(logger.error(s"教师Token验证失败: teacherToken=${teacherToken}")) >>
            IO.raiseError(new IllegalArgumentException(s"教师Token[${teacherToken}]验证失败"))
      }

      // Step 2: Fetch course group and validate existence and ownership
      courseGroup <- fetchCourseGroupByID(courseGroupID).flatMap {
        case Some(group) if group.ownerTeacherID == teacherID =>
          IO(logger.info(s"课程组验证通过 - 存在且归属教师ID匹配: courseGroup=${group}")).as(group)
        case Some(_) =>
          IO(logger.error(s"课程组归属教师ID不匹配: ownerTeacherID=${_.ownerTeacherID}")) >>
            IO.raiseError(new IllegalArgumentException(s"课程组归属权验证失败: courseGroupID[${courseGroupID}]"))
        case None =>
          IO(logger.error(s"课程组不存在: courseGroupID=${courseGroupID}")) >>
            IO.raiseError(new IllegalArgumentException(s"课程组[${courseGroupID}]不存在"))
      }

      // Step 3: validate teacher manage permission
      _ <- validateTeacherManagePermission().flatMap { hasPermission =>
        if (!hasPermission) {
          IO(logger.error(s"教师管理权限未开启")) >>
            IO.raiseError(new IllegalStateException(s"教师管理权限未开启"))
        } else IO(logger.info(s"教师管理权限验证通过"))
      }

      // Step 4: Update AuthorizedTeachersTable
      alreadyAuthorized = courseGroup.authorizedTeachers.contains(authorizedTeacherID)
      _ <- if (!alreadyAuthorized) {
        val insertSql = s"""
          INSERT INTO ${schemaName}.authorized_teachers_table (course_group_id, authorized_teacher_id)
          VALUES (?, ?)
        """
        writeDB(insertSql, List(
          SqlParameter("Int", courseGroupID.toString),
          SqlParameter("Int", authorizedTeacherID.toString)
        )).flatMap(_ => IO(logger.info(s"授权更新成功: authorizedTeacherID=${authorizedTeacherID}")))
      } else IO(logger.info(s"授权教师已存在，无需更新: authorizedTeacherID=${authorizedTeacherID}"))

      // Step 5: Record operation in log
      _ <- recordCourseGroupOperationLog(
        teacherID = teacherID,
        operation = "GrantAuthorization",
        courseGroupID = courseGroupID,
        details = s"授权教师ID: ${authorizedTeacherID}"
      )

      // Step 6: Fetch updated authorized teachers
      updatedAuthorizedTeacherIDs <- {
        val updatedAuthorizedTeachersSql = s"""
          SELECT authorized_teacher_id
          FROM ${schemaName}.authorized_teachers_table
          WHERE course_group_id = ?
        """
        readDBRows(updatedAuthorizedTeachersSql, List(SqlParameter("Int", courseGroupID.toString)))
          .map(rows => rows.map(json => decodeField[Int](json, "authorized_teacher_id")))
      }

      _ <- IO(logger.info(s"更新后的授权教师ID列表: ${updatedAuthorizedTeacherIDs.mkString(", ")}"))
    } yield updatedAuthorizedTeacherIDs
  }
}