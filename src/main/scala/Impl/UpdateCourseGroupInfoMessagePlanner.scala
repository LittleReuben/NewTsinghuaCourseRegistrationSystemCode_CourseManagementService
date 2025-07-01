package Impl


import APIs.UserAuthService.VerifyTokenValidityMessage
import Utils.CourseManagementProcess.recordCourseGroupOperationLog
import Utils.CourseManagementProcess.fetchCourseGroupByID
import Objects.SystemLogService.SystemLogEntry
import Objects.UserAccountService.SafeUserInfo
import Objects.UserAccountService.UserRole
import Utils.CourseManagementProcess.validateTeacherToken
import Objects.CourseManagementService.CourseGroup
import Utils.CourseManagementProcess.validateTeacherManagePermission
import Common.API.{PlanContext, Planner}
import Common.DBAPI._
import Common.Object.SqlParameter
import Common.ServiceUtils.schemaName
import cats.effect.IO
import org.slf4j.LoggerFactory
import org.joda.time.DateTime
import io.circe._
import io.circe.syntax._
import io.circe.generic.auto._
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
import Utils.CourseManagementProcess.validateTeacherManagePermission
import Common.Serialize.CustomColumnTypes.{decodeDateTime,encodeDateTime}

case class UpdateCourseGroupInfoMessagePlanner(
    teacherToken: String,
    courseGroupID: Int,
    newName: Option[String],
    newCredit: Option[Int],
    override val planContext: PlanContext
) extends Planner[CourseGroup] {
  val logger = LoggerFactory.getLogger(this.getClass.getSimpleName + "_" + planContext.traceID.id)

  override def plan(using planContext: PlanContext): IO[CourseGroup] = {
    for {
      // Step 1: Validate teacherToken
      _ <- IO(logger.info(s"[Step 1] 验证教师Token: ${teacherToken}"))
      maybeTeacherID <- validateTeacherToken(teacherToken)
      teacherID <- maybeTeacherID match {
        case Some(validTeacherID) =>
          IO(logger.info(s"教师ID验证成功: ${validTeacherID}")).as(validTeacherID)
        case None =>
          IO(logger.error(s"教师Token验证失败: ${teacherToken}")) >>
          IO.raiseError(new IllegalArgumentException(s"教师Token[${teacherToken}]无效"))
      }

      // Step 2: Fetch course group by ID and validate ownership
      _ <- IO(logger.info(s"[Step 2] 验证课程组是否存在以及是否与教师ID匹配: courseGroupID=${courseGroupID}, teacherID=${teacherID}"))
      maybeCourseGroup <- fetchCourseGroupByID(courseGroupID)
      courseGroup <- maybeCourseGroup match {
        case Some(group) if group.ownerTeacherID == teacherID =>
          IO(logger.info(s"课程组验证成功: ${group}")).as(group)
        case Some(group) =>
          IO(logger.error(s"课程组属主ID与教师ID不匹配: courseGroupID=${courseGroupID}, teacherID=${teacherID}, ownerTeacherID=${group.ownerTeacherID}")) >>
          IO.raiseError(new IllegalArgumentException(s"课程组属主ID[${group.ownerTeacherID}]与教师ID[${teacherID}]不匹配"))
        case None =>
          IO(logger.error(s"课程组不存在: courseGroupID=${courseGroupID}")) >>
          IO.raiseError(new IllegalStateException(s"课程组[${courseGroupID}]不存在"))
      }

      // Step 3: Validate teacher permission to manage the course group
      _ <- IO(logger.info(s"[Step 3] 验证教师是否有修改课程组的权限"))
      hasPermission <- validateTeacherManagePermission()
      _ <- if (hasPermission) IO(logger.info(s"教师具有修改权限"))
           else IO.raiseError(new IllegalStateException(s"当前阶段禁止教师修改课程组"))

      // Step 4: Update course group information in the database
      _ <- IO(logger.info(s"[Step 4] 更新课程组信息: courseGroupID=${courseGroupID}, newName=${newName}, newCredit=${newCredit}"))
      updateNameSQL <- IO {
        newName.map(name =>
          s"UPDATE ${schemaName}.course_group_table SET name = ? WHERE course_group_id = ?"
        )
      }
      updateCreditSQL <- IO {
        newCredit.map(credit =>
          s"UPDATE ${schemaName}.course_group_table SET credit = ? WHERE course_group_id = ?"
        )
      }
      _ <- updateNameSQL match {
        case Some(sql) =>
          writeDB(sql, List(SqlParameter("String", newName.get), SqlParameter("Int", courseGroupID.toString)))
        case None => IO.unit
      }
      _ <- updateCreditSQL match {
        case Some(sql) =>
          writeDB(sql, List(SqlParameter("Int", newCredit.get.toString), SqlParameter("Int", courseGroupID.toString)))
        case None => IO.unit
      }

      // Step 5: Re-fetch the updated course group from the database
      _ <- IO(logger.info(s"[Step 5] 重新查询更新后的课程组信息: courseGroupID=${courseGroupID}"))
      updatedCourseGroupOpt <- fetchCourseGroupByID(courseGroupID)
      updatedCourseGroup <- updatedCourseGroupOpt match {
        case Some(group) =>
          IO(logger.info(s"查询成功: ${group}")).as(group)
        case None =>
          IO(logger.error(s"查询失败: 更新后的课程组不存在: courseGroupID=${courseGroupID}")) >>
          IO.raiseError(new IllegalStateException(s"更新后的课程组[${courseGroupID}]不存在"))
      }

      // Step 6: Record operation log
      _ <- IO(logger.info(s"[Step 6] 记录操作日志"))
      operationDetails = s"更新课程组: ID=${courseGroupID}, 新名称=${newName.getOrElse("<无变更>")}, 新学分=${newCredit.getOrElse("<无变更>")}"
      _ <- recordCourseGroupOperationLog(teacherID, "UpdateCourseGroup", courseGroupID, operationDetails)

      _ <- IO(logger.info(s"API 执行成功: 返回更新后的课程组信息: ${updatedCourseGroup}"))
    } yield updatedCourseGroup
  }
}