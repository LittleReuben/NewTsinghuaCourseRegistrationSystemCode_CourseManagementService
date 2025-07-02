package Impl

// Imports 合并重复导入并维持必要模块

import Common.API.{PlanContext, Planner}
import Common.DBAPI._
import Common.Object.SqlParameter
import Common.ServiceUtils.schemaName
import Objects.CourseManagementService.CourseGroup
import Objects.SystemLogService.SystemLogEntry
import Utils.CourseManagementProcess._
import cats.effect.IO
import org.joda.time.DateTime
import org.slf4j.LoggerFactory
import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._
import cats.implicits._
import Common.Serialize.CustomColumnTypes.{decodeDateTime, encodeDateTime}
import APIs.UserAuthService.VerifyTokenValidityMessage
import Objects.UserAccountService.{UserRole, SafeUserInfo}

case class UpdateCourseGroupInfoMessagePlanner(
    teacherToken: String,
    courseGroupID: Int,
    newName: Option[String],
    newCredit: Option[Int],
    override val planContext: PlanContext
) extends Planner[CourseGroup] {
  private val logger = LoggerFactory.getLogger(this.getClass.getSimpleName + "_" + planContext.traceID.id)

  override def plan(using planContext: PlanContext): IO[CourseGroup] = {
    for {
      // Step 1: 验证教师 Token 有效性
      teacherID <- validateTeacherTokenStep()

      // Step 2: 查询课程组信息并验证
      originalCourseGroup <- fetchAndValidateCourseGroupStep(teacherID)

      // Step 3: 验证教师是否有权限修改课程组
      _ <- validateModifyPermissionStep()

      // Step 4: 更新课程组信息
      _ <- updateCourseGroupStep(originalCourseGroup)

      // Step 5: 记录操作日志
      _ <- recordOperationLogStep(teacherID, originalCourseGroup)

      // Step 6: 获取更新后的课程组信息
      updatedCourseGroup <- fetchUpdatedCourseGroupStep()

    } yield updatedCourseGroup
  }

  private def validateTeacherTokenStep()(using PlanContext): IO[Int] = {
    for {
      _ <- IO(logger.info(s"开始验证教师 token: ${teacherToken}"))
      teacherIDOpt <- validateTeacherToken(teacherToken)
      teacherID <- teacherIDOpt match {
        case None =>
          IO(logger.error(s"教师 token 无效: ${teacherToken}")) >>
            IO.raiseError(new IllegalArgumentException("Invalid teacher token"))
        case Some(id) =>
          IO(logger.info(s"教师 token 验证成功, teacherID=${id}")).as(id)
      }
    } yield teacherID
  }

  private def fetchAndValidateCourseGroupStep(teacherID: Int)(using PlanContext): IO[CourseGroup] = {
    for {
      _ <- IO(logger.info(s"开始查询课程组信息, courseGroupID=${courseGroupID}"))
      courseGroupOpt <- fetchCourseGroupByID(courseGroupID)
      courseGroup <- courseGroupOpt match {
        case None =>
          IO(logger.error(s"课程组不存在, courseGroupID=${courseGroupID}")) >>
            IO.raiseError(new IllegalArgumentException("Course group not found"))
        case Some(group) if group.ownerTeacherID != teacherID =>
          IO(logger.error(s"课程组不属于该教师ID=${teacherID}, courseGroupID=${courseGroupID}")) >>
            IO.raiseError(new IllegalArgumentException("Teacher does not own the course group"))
        case Some(group) =>
          IO(logger.info(s"课程组验证成功, courseGroup=${group}")).as(group)
      }
    } yield courseGroup
  }

  private def validateModifyPermissionStep()(using PlanContext): IO[Unit] = {
    for {
      _ <- IO(logger.info("开始验证教师的修改权限"))
      canModify <- validateTeacherManagePermission()
      _ <- if (!canModify) {
        IO(logger.error("当前阶段不允许教师修改课程组")) >>
          IO.raiseError(new IllegalStateException("Permission denied"))
      } else {
        IO(logger.info("当前阶段允许教师修改课程组"))
      }
    } yield ()
  }

  private def updateCourseGroupStep(originalGroup: CourseGroup)(using PlanContext): IO[Unit] = {
    for {
      updateFields = {
        val nameUpdate = newName.map(name => s"name = '${name}'")
        val creditUpdate = newCredit.map(credit => s"credit = ${credit}")
        Seq(nameUpdate, creditUpdate).flatten.mkString(", ")
      }

      _ <- if (updateFields.isEmpty) {
        IO(logger.warn("未传入任何需要更新的字段")) >>
          IO.raiseError(new IllegalArgumentException("No update fields provided"))
      } else {
        IO(logger.info(s"待更新字段: ${updateFields}"))
      }

      updateSQL = s"""
        UPDATE ${schemaName}.course_group_table
        SET ${updateFields}
        WHERE course_group_id = ?;
      """

      _ <- writeDB(
        updateSQL,
        List(SqlParameter("Int", courseGroupID.toString))
      ).flatMap(result => IO(logger.info(s"课程组信息更新完成: ${result}")))
    } yield ()
  }

  private def recordOperationLogStep(teacherID: Int, originalGroup: CourseGroup)(using PlanContext): IO[Unit] = {
    val details = s"Updated course group: ID=${originalGroup.courseGroupID}, newName=${newName.getOrElse("unchanged")}, newCredit=${newCredit.getOrElse(-1)}"
    recordCourseGroupOperationLog(teacherID, "UpdateCourseGroupInfo", courseGroupID, details)
  }

  private def fetchUpdatedCourseGroupStep()(using PlanContext): IO[CourseGroup] = {
    for {
      _ <- IO(logger.info("开始获取更新后的课程组信息"))
      updatedCourseGroupOpt <- fetchCourseGroupByID(courseGroupID)
      updatedCourseGroup <- updatedCourseGroupOpt match {
        case None =>
          IO(logger.error(s"无法获取更新后的课程组信息, courseGroupID=${courseGroupID}")) >>
            IO.raiseError(new IllegalStateException("Failed to fetch updated course group"))
        case Some(group) =>
          IO(logger.info(s"获取更新后的课程组信息成功: ${group}")).as(group)
      }
    } yield updatedCourseGroup
  }
}