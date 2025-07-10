package Impl


import Common.API.{PlanContext, Planner}
import Common.DBAPI._
import Common.Object.SqlParameter
import Common.ServiceUtils.schemaName
import Objects.SystemLogService.SystemLogEntry
import Objects.UserAccountService.SafeUserInfo
import Objects.CourseManagementService.CourseGroup
import Utils.CourseManagementProcess._
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
import Utils.CourseManagementProcess.recordCourseGroupOperationLog
import APIs.UserAuthService.VerifyTokenValidityMessage
import Utils.CourseManagementProcess.fetchCourseGroupByID
import Objects.UserAccountService.UserRole
import Utils.CourseManagementProcess.validateTeacherToken
import Utils.CourseManagementProcess.validateTeacherManagePermission

case class DeleteCourseGroupMessagePlanner(
  teacherToken: String,
  courseGroupID: Int,
  override val planContext: PlanContext
) extends Planner[String] {
  val logger = LoggerFactory.getLogger(this.getClass.getSimpleName + "_" + planContext.traceID.id)

  override def plan(using PlanContext): IO[String] = {
    for {
      // Step 1: Validate teacher token and fetch teacher ID
      _ <- IO(logger.info(s"开始验证教师Token: ${teacherToken}"))
      teacherIDOpt <- validateTeacherToken(teacherToken)
      teacherID <- teacherIDOpt match {
        case Some(id) => IO(logger.info(s"教师Token验证通过，教师ID: ${id}")).as(id)
        case None =>
          IO(logger.error(s"教师Token验证失败: ${teacherToken}")) >>
          IO.raiseError(new IllegalArgumentException("教师Token验证失败"))
      }

      // Step 2: Fetch CourseGroup by courseGroupID
      _ <- IO(logger.info(s"开始查询课程组信息，courseGroupID: ${courseGroupID}"))
      courseGroupOpt <- fetchCourseGroupByID(courseGroupID)
      courseGroup <- courseGroupOpt match {
        case Some(group) => IO(logger.info(s"查询到课程组信息: ${group}")).as(group)
        case None =>
          IO(logger.error(s"课程组不存在，courseGroupID: ${courseGroupID}")) >>
          IO.raiseError(new IllegalArgumentException("课程组不存在"))
      }

      // Step 3: Verify courseGroup ownership
      _ <- if (courseGroup.ownerTeacherID != teacherID)
        IO(logger.error(s"教师无权操作课程组，教师ID: ${teacherID}, 课程组责任教师ID: ${courseGroup.ownerTeacherID}")) >>
        IO.raiseError(new IllegalArgumentException("教师无权操作课程组"))
      else IO(logger.info(s"验证课程组归属权成功，教师ID: ${teacherID} 是课程组的责任教师"))

      // Step 4: Validate permission to delete course group
      _ <- IO(logger.info(s"开始验证教师的课程组删除权限"))
      canDelete <- validateTeacherManagePermission()
      _ <- if (!canDelete)
        IO(logger.error(s"当前阶段不允许删除课程组")) >>
        IO.raiseError(new IllegalArgumentException("操作课程权限未开启！"))
      else IO(logger.info(s"当前阶段允许删除课程组"))

      // Step 5: Delete associated courses from CourseTable
      _ <- IO(logger.info(s"删除与课程组相关的所有课程, courseGroupID: ${courseGroupID}"))
      deleteCoursesResult <- writeDB(
        s"DELETE FROM ${schemaName}.course_table WHERE course_group_id = ?",
        List(SqlParameter("Int", courseGroupID.toString))
      )
      _ <- IO(logger.info(s"课程删除结果: ${deleteCoursesResult}"))

      // Step 6: Delete associated authorized teachers from AuthorizedTeachersTable
      _ <- IO(logger.info(s"删除与课程组相关的授权教师记录, courseGroupID: ${courseGroupID}"))
      deleteAuthTeachersResult <- writeDB(
        s"DELETE FROM ${schemaName}.authorized_teachers_table WHERE course_group_id = ?",
        List(SqlParameter("Int", courseGroupID.toString))
      )
      _ <- IO(logger.info(s"授权教师记录删除结果: ${deleteAuthTeachersResult}"))

      // Step 7: Delete the course group record from CourseGroupTable
      _ <- IO(logger.info(s"删除课程组记录, courseGroupID: ${courseGroupID}"))
      deleteCourseGroupResult <- writeDB(
        s"DELETE FROM ${schemaName}.course_group_table WHERE course_group_id = ?",
        List(SqlParameter("Int", courseGroupID.toString))
      )
      _ <- IO(logger.info(s"课程组记录删除结果: ${deleteCourseGroupResult}"))

      // Step 8: Record operation log
      _ <- IO(logger.info(s"记录课程组删除操作日志"))
      _ <- recordCourseGroupOperationLog(
        teacherID = teacherID,
        operation = "DeleteCourseGroup",
        courseGroupID = courseGroupID,
        details = s"教师ID: ${teacherID} 删除了课程组ID: ${courseGroupID}"
      )
      _ <- IO(logger.info("操作日志记录完成"))

    } yield "删除成功！"
  }
}