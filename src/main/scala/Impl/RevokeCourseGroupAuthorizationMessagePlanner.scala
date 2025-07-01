package Impl


import Common.API.{PlanContext, Planner}
import Common.DBAPI._
import Common.Object.ParameterList
import Common.Object.SqlParameter
import Common.ServiceUtils.schemaName
import Objects.CourseManagementService.CourseGroup
import Utils.CourseManagementProcess._
import Objects.SystemLogService.SystemLogEntry
import Objects.UserAccountService.SafeUserInfo
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
import Utils.CourseManagementProcess.recordCourseGroupOperationLog
import APIs.UserAuthService.VerifyTokenValidityMessage
import Utils.CourseManagementProcess.checkAuthorizationForTeacher
import Utils.CourseManagementProcess.fetchCourseGroupByID
import Objects.UserAccountService.UserRole
import Utils.CourseManagementProcess.validateTeacherToken
import Utils.CourseManagementProcess.validateTeacherManagePermission
import cats.implicits.*
import Common.Serialize.CustomColumnTypes.{decodeDateTime,encodeDateTime}
import Utils.CourseManagementProcess.validateTeacherManagePermission

case class RevokeCourseGroupAuthorizationMessagePlanner(
    teacherToken: String,
    courseGroupID: Int,
    authorizedTeacherID: Int,
    override val planContext: PlanContext
) extends Planner[List[Int]] {

  val logger = LoggerFactory.getLogger(this.getClass.getSimpleName + "_" + planContext.traceID.id)

  override def plan(using planContext: PlanContext): IO[List[Int]] = {

    for {
      // Step 1: 验证老师令牌并获取老师ID。
      _ <- IO(logger.info(s"[Step 1] 验证老师令牌中: teacherToken=${teacherToken}"))
      teacherIDOpt <- validateTeacherToken(teacherToken)
      teacherID <- teacherIDOpt match {
        case Some(id) =>
          IO(logger.info(s"[Step 1.1] 教师令牌验证通过, teacherID=${id}")).map(_ => id)
        case None =>
          IO(logger.error("[Step 1.2] 教师令牌验证失败")) >>
          IO.raiseError(new IllegalArgumentException("教师鉴权失败"))
      }

      // Step 2: 确认课程组的存在及操作权限。
      _ <- IO(logger.info(s"[Step 2] 确认课程组存在及权限验证中: courseGroupID=${courseGroupID}"))
      courseGroupOpt <- fetchCourseGroupByID(courseGroupID)
      courseGroup <- courseGroupOpt match {
        case Some(group) =>
          if (group.ownerTeacherID == teacherID) IO.pure(group)
          else
            IO(logger.error(s"[Step 2.1] 教师不是课程组的创建者，无权操作: courseGroupID=${courseGroupID}, ownerTeacherID=${group.ownerTeacherID}")) >>
            IO.raiseError(new IllegalArgumentException("无权操作该课程组"))
        case None =>
          IO(logger.error(s"[Step 2.2] 课程组不存在: courseGroupID=${courseGroupID}")) >>
          IO.raiseError(new IllegalArgumentException("无权操作该课程组"))
      }

      // Step 3: 验证当前阶段是否允许取消授权。
      _ <- IO(logger.info("[Step 3] 验证当前阶段权限"))
      isManageAllowed <- validateTeacherManagePermission()
      _ <- if (!isManageAllowed) {
        IO(logger.error("[Step 3.1] 当前阶段不允许取消授权")) >>
        IO.raiseError(new IllegalArgumentException("当前阶段不允许取消授权"))
      } else IO(logger.info("[Step 3.2] 当前阶段允许操作"))

      // Step 4: 删除授权记录。
      _ <- IO(logger.info(s"[Step 4] 删除授权记录中: authorizedTeacherID=${authorizedTeacherID}, courseGroupID=${courseGroupID}"))
      _ <- writeDB(
        s"DELETE FROM ${schemaName}.authorized_teachers_table WHERE authorized_teacher_id = ? AND course_group_id = ?",
        List(
          SqlParameter("Int", authorizedTeacherID.toString),
          SqlParameter("Int", courseGroupID.toString)
        )
      )

      // Step 5: 删除与该老师创建的所有关联课程。
      _ <- IO(logger.info(s"[Step 5] 删除与该教师创建的所有关联课程: authorizedTeacherID=${authorizedTeacherID}, courseGroupID=${courseGroupID}"))
      _ <- writeDB(
        s"DELETE FROM ${schemaName}.course_table WHERE teacher_id = ? AND course_group_id = ?",
        List(
          SqlParameter("Int", authorizedTeacherID.toString),
          SqlParameter("Int", courseGroupID.toString)
        )
      )

      // Step 6: 记录取消授权操作日志。
      _ <- IO(logger.info("[Step 6] 记录取消授权操作日志"))
      logDetails = s"教师ID[${teacherID}]取消了教师ID[${authorizedTeacherID}]在课程组[${courseGroupID}]的授权"
      _ <- recordCourseGroupOperationLog(teacherID, "取消授权", courseGroupID, logDetails)

      // Step 7: 查询课程组最新授权教师列表并返回。
      _ <- IO(logger.info("[Step 7] 查询课程组的最新授权教师列表"))
      authorizedTeachersRows <- readDBRows(
        s"SELECT authorized_teacher_id FROM ${schemaName}.authorized_teachers_table WHERE course_group_id = ?",
        List(SqlParameter("Int", courseGroupID.toString))
      )
      authorizedTeachers <- IO {
        authorizedTeachersRows.map(json => decodeField[Int](json, "authorized_teacher_id"))
      }
      _ <- IO(logger.info(s"[Step 7.1] 更新后的授权教师列表: ${authorizedTeachers.mkString(", ")}"))

    } yield authorizedTeachers
  }
}