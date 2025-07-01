package Impl


import Common.API.{PlanContext, Planner}
import Common.DBAPI._
import Common.Object.SqlParameter
import Common.ServiceUtils.schemaName
import Objects.UserAccountService.SafeUserInfo
import Utils.CourseManagementProcess.recordCourseGroupOperationLog
import Objects.CourseManagementService.CourseGroup
import Utils.CourseManagementProcess.fetchCourseGroupByID
import Utils.CourseManagementProcess.validateTeacherToken
import Utils.CourseManagementProcess.validateTeacherManagePermission
import io.circe._
import io.circe.syntax._
import io.circe.generic.auto._
import org.joda.time.DateTime
import org.slf4j.LoggerFactory
import cats.effect.IO
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
import APIs.UserAuthService.VerifyTokenValidityMessage
import Objects.SystemLogService.SystemLogEntry
import Objects.UserAccountService.UserRole
import Utils.CourseManagementProcess.validateTeacherManagePermission
import Utils.CourseManagementProcess._
import cats.implicits.*
import Common.Serialize.CustomColumnTypes.{decodeDateTime,encodeDateTime}

case class GrantCourseGroupAuthorizationMessagePlanner(
    teacherToken: String,
    courseGroupID: Int,
    authorizedTeacherID: Int,
    override val planContext: PlanContext
) extends Planner[List[Int]] {
  private val logger = LoggerFactory.getLogger(this.getClass.getSimpleName + "_" + planContext.traceID.id)

  override def plan(using PlanContext): IO[List[Int]] = {
    for {
      // Step 1: Validate teacher token
      _ <- IO(logger.info(s"验证教师 token: ${teacherToken}"))
      teacherIDOpt <- validateTeacherToken(teacherToken)
      teacherID <- teacherIDOpt match {
        case Some(id) =>
          IO(logger.info(s"教师 token 验证通过, teacherID=${id}")).as(id)
        case None =>
          IO(logger.error(s"教师 token 无效或鉴权失败")) >>
            IO.raiseError(new IllegalArgumentException("Invalid teacherToken"))
      }

      // Step 2: Verify course group existence and ownership
      _ <- IO(logger.info(s"查询课程组信息, courseGroupID=${courseGroupID}"))
      courseGroupOpt <- fetchCourseGroupByID(courseGroupID)
      courseGroup <- courseGroupOpt match {
        case Some(group) if group.ownerTeacherID == teacherID =>
          IO(logger.info(s"课程组信息验证通过: ${group}")).as(group)
        case Some(group) =>
          IO(logger.error(s"课程组所有者 ID 与当前教师 ID 不匹配: ownerTeacherID=${group.ownerTeacherID}, teacherID=${teacherID}")) >>
            IO.raiseError(new IllegalArgumentException("Course group ownership mismatch"))
        case None =>
          IO(logger.error(s"指定课程组不存在: courseGroupID=${courseGroupID}")) >>
            IO.raiseError(new IllegalArgumentException("Course group not found"))
      }

      // Step 3: Verify teacher's manage permission
      _ <- IO(logger.info(s"验证教师管理权限"))
      canManage <- validateTeacherManagePermission()
      _ <- if (canManage) IO(logger.info("权限验证通过"))
           else IO(logger.error("当前阶段没有权限管理教师")) >> IO.raiseError(new IllegalStateException("Manage permission denied"))

      // Step 4: Update authorization table
      _ <- IO(logger.info(s"将 authorizedTeacherID=${authorizedTeacherID} 添加到课程组的已授权教师列表"))
      existingAuthorizedTeachers = courseGroup.authorizedTeachers
      _ <- if (existingAuthorizedTeachers.contains(authorizedTeacherID)) {
        IO(logger.info(s"教师 ID ${authorizedTeacherID} 已被授权，跳过更新"))
      } else {
        val sql = s"""
          INSERT INTO ${schemaName}.authorized_teachers_table (course_group_id, authorized_teacher_id)
          VALUES (?, ?)
        """
        writeDB(sql, List(
          SqlParameter("Int", courseGroupID.toString),
          SqlParameter("Int", authorizedTeacherID.toString)
        )) >> IO(logger.info(s"已更新授权教师表: courseGroupID=${courseGroupID}, authorizedTeacherID=${authorizedTeacherID}"))
      }

      // Step 5: Record operation log
      _ <- IO(logger.info(s"记录授权操作日志: teacherID=${teacherID}, courseGroupID=${courseGroupID}, authorizedTeacherID=${authorizedTeacherID}"))
      _ <- recordCourseGroupOperationLog(
        teacherID = teacherID,
        operation = "Grant Authorization",
        courseGroupID = courseGroupID,
        details = s"Authorized Teacher ID: ${authorizedTeacherID}"
      )

      // Step 6: Retrieve updated authorized teachers list
      _ <- IO(logger.info(s"获取更新后的授权教师 ID 列表, courseGroupID=${courseGroupID}"))
      updatedAuthorizedTeachers <- fetchCourseGroupByID(courseGroupID).map(_.map(_.authorizedTeachers).getOrElse(Nil))
      _ <- IO(logger.info(s"更新后的授权教师 ID 列表: ${updatedAuthorizedTeachers}"))
    } yield updatedAuthorizedTeachers
  }
}