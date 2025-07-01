package Impl


import Utils.CourseManagementProcess.recordCourseGroupOperationLog
import Utils.CourseManagementProcess.checkAuthorizationForTeacher
import Utils.CourseManagementProcess.fetchCourseGroupByID
import Utils.CourseManagementProcess.validateTeacherToken
import Utils.CourseManagementProcess.validateTeacherManagePermission
import Objects.CourseManagementService.CourseGroup
import Common.Object.SqlParameter
import Common.API.{PlanContext, Planner}
import Common.DBAPI._
import Common.ServiceUtils.schemaName
import cats.effect.IO
import org.slf4j.LoggerFactory
import org.joda.time.DateTime
import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._
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
import Objects.UserAccountService.SafeUserInfo
import Utils.CourseManagementProcess.validateTeacherManagePermission
import cats.implicits.*
import Common.Serialize.CustomColumnTypes.{decodeDateTime,encodeDateTime}

case class RevokeCourseGroupAuthorizationMessagePlanner(
    teacherToken: String,
    courseGroupID: Int,
    authorizedTeacherID: Int,
    override val planContext: PlanContext
) extends Planner[List[Int]] {

  private val logger = LoggerFactory.getLogger(this.getClass.getSimpleName + "_" + planContext.traceID.id)

  override def plan(using PlanContext): IO[List[Int]] = {
    for {
      // Step 1: Validate teacher token and get teacherID
      teacherID <- validateTeacherTokenAndLog()

      // Step 2: Validate course group existence and permissions
      courseGroup <- validateCourseGroupAndPermissions(teacherID)

      // Step 3: Validate current permission to revoke authorization
      _ <- validateRevokePermission()

      // Step 4: Remove authorization record
      _ <- removeTeacherAuthorization(courseGroupID)

      // Step 5: Remove courses created by the authorized teacher in the course group
      _ <- removeCoursesLinkedToTeacher(courseGroupID)

      // Step 6: Record operation log for revoke action
      _ <- recordOperationLog(teacherID, courseGroupID)

      // Step 7: Return updated authorized teachers list
      updatedAuthorizedTeachers <- fetchUpdatedAuthorizedTeachers(courseGroupID)

    } yield updatedAuthorizedTeachers
  }

  private def validateTeacherTokenAndLog()(using PlanContext): IO[Int] = {
    for {
      _ <- IO(logger.info("[Step 1] 开始验证教师令牌"))
      teacherIDOpt <- validateTeacherToken(teacherToken)
      teacherID <- teacherIDOpt match {
        case Some(id) =>
          IO(logger.info(s"[Step 1] 教师令牌验证通过, teacherID=${id}")).as(id)
        case None =>
          IO(logger.error("[Step 1] 教师令牌验证失败")) >>
          IO.raiseError(new IllegalArgumentException("教师鉴权失败"))
      }
    } yield teacherID
  }

  private def validateCourseGroupAndPermissions(teacherID: Int)(using PlanContext): IO[CourseGroup] = {
    for {
      _ <- IO(logger.info("[Step 2] 开始验证课程组信息及操作权限"))
      courseGroupOpt <- fetchCourseGroupByID(courseGroupID)
      courseGroup <- courseGroupOpt match {
        case Some(group) if group.ownerTeacherID == teacherID =>
          IO(logger.info(s"[Step 2] 课程组验证通过，课程组信息=${group}")).as(group)
        case Some(_) =>
          IO(logger.error(s"[Step 2] 教师无权操作该课程组, courseGroupID=${courseGroupID}")) >>
          IO.raiseError(new IllegalArgumentException("无权操作该课程组"))
        case None =>
          IO(logger.error(s"[Step 2] 课程组不存在, courseGroupID=${courseGroupID}")) >>
          IO.raiseError(new IllegalArgumentException("无权操作该课程组"))
      }
    } yield courseGroup
  }

  private def validateRevokePermission()(using PlanContext): IO[Unit] = {
    for {
      _ <- IO(logger.info("[Step 3] 验证当前阶段是否允许取消授权"))
      canRevoke <- validateTeacherManagePermission()
      _ <- if (canRevoke) IO(logger.info("[Step 3] 当前阶段允许取消授权"))
           else IO(logger.error("[Step 3] 当前阶段不允许取消授权")) >>
           IO.raiseError(new IllegalArgumentException("当前阶段不允许取消授权"))
    } yield ()
  }

  private def removeTeacherAuthorization(courseGroupID: Int)(using PlanContext): IO[Unit] = {
    val deleteAuthSql =
      s"""
         DELETE FROM ${schemaName}.authorized_teachers_table
         WHERE course_group_id = ? AND authorized_teacher_id = ?
       """
    for {
      _ <- IO(logger.info("[Step 4] 开始删除授权记录"))
      deleteAuthResult <- writeDB(
        deleteAuthSql,
        List(
          SqlParameter("Int", courseGroupID.toString),
          SqlParameter("Int", authorizedTeacherID.toString)
        )
      )
      _ <- IO(logger.info(s"[Step 4] 授权记录删除完成，结果=${deleteAuthResult}"))
    } yield ()
  }

  private def removeCoursesLinkedToTeacher(courseGroupID: Int)(using PlanContext): IO[Unit] = {
    val deleteCoursesSql =
      s"""
         DELETE FROM ${schemaName}.course_table
         WHERE course_group_id = ? AND teacher_id = ?
       """
    for {
      _ <- IO(logger.info("[Step 5] 开始删除教师创建的所有关联课程"))
      deleteCoursesResult <- writeDB(
        deleteCoursesSql,
        List(
          SqlParameter("Int", courseGroupID.toString),
          SqlParameter("Int", authorizedTeacherID.toString)
        )
      )
      _ <- IO(logger.info(s"[Step 5] 关联课程删除完成，结果=${deleteCoursesResult}"))
    } yield ()
  }

  private def recordOperationLog(teacherID: Int, courseGroupID: Int)(using PlanContext): IO[Unit] = {
    for {
      _ <- IO(logger.info("[Step 6] 开始记录取消授权操作日志"))
      details = s"取消授权教师ID ${authorizedTeacherID} 针对课程组ID ${courseGroupID}"
      _ <- recordCourseGroupOperationLog(teacherID, "取消授权", courseGroupID, details)
    } yield ()
  }

  private def fetchUpdatedAuthorizedTeachers(courseGroupID: Int)(using PlanContext): IO[List[Int]] = {
    for {
      _ <- IO(logger.info("[Step 7] 获取课程组的最新授权老师列表"))
      updatedAuthorizedTeachers <- checkAuthorizationForTeacher(courseGroupID)
      _ <- IO(logger.info(s"[Step 7] 最新授权老师列表: ${updatedAuthorizedTeachers.mkString(", ")}"))
    } yield updatedAuthorizedTeachers
  }
}