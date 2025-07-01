package Impl


import Objects.CourseManagementService.CourseGroup
import Objects.UserAccountService.UserRole
import Objects.UserAccountService.SafeUserInfo
import Objects.SystemLogService.SystemLogEntry
import Common.API.{PlanContext, Planner}
import Common.DBAPI._
import Common.Object.SqlParameter
import Common.ServiceUtils.schemaName
import cats.effect.IO
import org.slf4j.LoggerFactory
import org.joda.time.DateTime
import Utils.CourseManagementProcess.validateTeacherToken
import Utils.CourseManagementProcess.validateTeacherManagePermission
import Utils.CourseManagementProcess.recordCourseGroupOperationLog
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
import Utils.CourseManagementProcess.fetchCourseGroupByID
import Utils.CourseManagementProcess.validateTeacherManagePermission
import io.circe._
import io.circe.syntax._
import io.circe.generic.auto._
import cats.implicits._
import cats.implicits.*
import Common.Serialize.CustomColumnTypes.{decodeDateTime,encodeDateTime}

case class CreateCourseGroupMessagePlanner(
    teacherToken: String,
    name: String,
    credit: Int,
    override val planContext: PlanContext
) extends Planner[CourseGroup] {
  val logger = LoggerFactory.getLogger(this.getClass.getSimpleName + "_" + planContext.traceID.id)

  override def plan(using PlanContext): IO[CourseGroup] = {
    for {
      // Step 1: 验证教师 Token 并获取 teacherID。
      _ <- IO(logger.info(s"[Step 1] 验证教师 token: ${teacherToken}"))
      teacherIDOpt <- validateTeacherToken(teacherToken)
      teacherID <- teacherIDOpt match {
        case Some(id) =>
          IO(logger.info(s"[Step 1.1] 教师 token 验证通过，teacherID=${id}")).as(id)
        case None =>
          IO(logger.error(s"[Step 1.1] 教师 token 无效或验证失败: ${teacherToken}")) >>
            IO.raiseError(new IllegalStateException("教师 token 验证失败"))
      }

      // Step 2: 检查当前阶段是否允许创建课程组。
      _ <- IO(logger.info(s"[Step 2] 检测当前阶段是否允许创建课程组"))
      canCreate <- validateTeacherManagePermission()
      _ <- if (canCreate) IO(logger.info("[Step 2.1] 权限验证通过，允许创建课程组"))
           else IO(logger.error("[Step 2.1] 权限验证失败，当前阶段禁止创建课程组")) >>
           IO.raiseError(new IllegalStateException("当前阶段禁止创建课程组"))

      // Step 3: 将 CourseGroup 数据插入 CourseGroupTable 中。
      _ <- IO(logger.info(s"[Step 3] 写入课程组信息至数据库: name=${name}, credit=${credit}, teacherID=${teacherID}"))
      insertCourseGroupSQL = s"""
        INSERT INTO ${schemaName}.course_group_table (name, credit, owner_teacher_id)
        VALUES (?, ?, ?)
        RETURNING course_group_id
      """
      courseGroupID <- readDBInt(
        insertCourseGroupSQL,
        List(
          SqlParameter("String", name),
          SqlParameter("Int", credit.toString),
          SqlParameter("Int", teacherID.toString)
        )
      )
      _ <- IO(logger.info(s"[Step 3.1] 课程组记录插入成功，生成的 courseGroupID=${courseGroupID}"))

      // Step 4: 添加授权教师记录到 AuthorizedTeachersTable。
      _ <- IO(logger.info(s"[Step 4] 添加授权教师信息，teacherID=${teacherID}, courseGroupID=${courseGroupID}"))
      insertAuthorizedTeacherSQL = s"""
        INSERT INTO ${schemaName}.authorized_teachers_table (course_group_id, authorized_teacher_id)
        VALUES (?, ?)
      """
      _ <- writeDB(
        insertAuthorizedTeacherSQL,
        List(
          SqlParameter("Int", courseGroupID.toString),
          SqlParameter("Int", teacherID.toString)
        )
      )
      _ <- IO(logger.info("[Step 4.1] 授权教师记录插入成功"))

      // Step 5: 记录课程组创建操作日志。
      _ <- IO(logger.info(s"[Step 5] 记录创建课程组操作日志"))
      details = s"创建课程组: 名称=${name}, 学分=${credit}, 教师ID=${teacherID}"
      _ <- recordCourseGroupOperationLog(teacherID, "CreateCourseGroup", courseGroupID, details)
      _ <- IO(logger.info("[Step 5.1] 操作日志记录成功"))

    } yield {
      // Step 6: 返回创建的课程组信息。
      val courseGroup = CourseGroup(
        courseGroupID = courseGroupID,
        name = name,
        credit = credit,
        ownerTeacherID = teacherID,
        authorizedTeachers = List(teacherID)
      )
      logger.info(s"[Step 6] 课程组创建成功，返回信息: ${courseGroup}")
      courseGroup
    }
  }
}