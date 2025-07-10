package Impl


import Common.API.{PlanContext, Planner}
import Common.DBAPI._
import Common.Object.ParameterList
import Common.Object.SqlParameter
import Common.ServiceUtils.schemaName
import Objects.CourseManagementService.CourseGroup
import Utils.CourseManagementProcess.recordCourseGroupOperationLog
import Utils.CourseManagementProcess.fetchCourseGroupByID
import Objects.SystemLogService.SystemLogEntry
import Objects.UserAccountService.UserRole
import Objects.UserAccountService.SafeUserInfo
import Utils.CourseManagementProcess.validateTeacherToken
import Utils.CourseManagementProcess.validateTeacherManagePermission
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
import APIs.UserAuthService.VerifyTokenValidityMessage
import Utils.CourseManagementProcess.validateTeacherManagePermission
import Common.Serialize.CustomColumnTypes.{decodeDateTime,encodeDateTime}

case class CreateCourseGroupMessagePlanner(
  teacherToken: String,
  name: String,
  credit: Int,
  override val planContext: PlanContext
) extends Planner[CourseGroup] {
  private val logger = LoggerFactory.getLogger(this.getClass.getSimpleName + "_" + planContext.traceID.id)

  override def plan(using planContext: PlanContext): IO[CourseGroup] = {
    for {
      // Step 1: 验证 Token 并获取教师 ID
      _ <- IO(logger.info(s"[Step 1] 验证教师 Token: ${teacherToken}"))
      teacherIDOpt <- validateTeacherToken(teacherToken)
      teacherID <- teacherIDOpt match {
        case Some(id) => 
          IO(logger.info(s"[Step 1.1] 教师 Token 验证通过，教师 ID: ${id}")).as(id)
        case None =>
          IO(logger.error(s"[Step 1.2] 教师 Token 验证失败: ${teacherToken}")) >>
          IO.raiseError(new IllegalArgumentException(s"教师 Token 无效或过期"))
      }

      // Step 2: 验证教师是否有权限创建课程组
      _ <- IO(logger.info(s"[Step 2] 验证当前阶段是否允许教师创建课程组"))
      canCreate <- validateTeacherManagePermission()
      _ <- if (!canCreate) {
        IO(logger.error("[Step 2.1] 当前阶段禁止教师创建课程组")) >>
        IO.raiseError(new IllegalStateException("创建课程组权限未开启！"))
      } else IO(logger.info("[Step 2.2] 教师权限验证通过"))

      // Step 3: 在 CourseGroupTable 中插入课程组数据
      _ <- IO(logger.info(s"[Step 3] 插入课程组数据：课程组名称=${name}, 学分=${credit}, 创建教师ID=${teacherID}"))
      insertCourseGroupSQL =
        s"""
          INSERT INTO ${schemaName}.course_group_table
          (name, credit, owner_teacher_id)
          VALUES (?, ?, ?)
          RETURNING course_group_id;
        """
      insertCourseGroupParams = List(
        SqlParameter("String", name),
        SqlParameter("Int", credit.toString),
        SqlParameter("Int", teacherID.toString)
      )
      courseGroupID <- readDBInt(insertCourseGroupSQL, insertCourseGroupParams)
      _ <- IO(logger.info(s"[Step 3.1] 成功插入课程组数据，生成的 courseGroupID: ${courseGroupID}"))

      // Step 4: 在 AuthorizedTeachersTable 中插入记录
      _ <- IO(logger.info(s"[Step 4] 为课程组添加默认授权教师"))
      insertAuthorizedTeacherSQL =
        s"""
          INSERT INTO ${schemaName}.authorized_teachers_table
          (course_group_id, authorized_teacher_id)
          VALUES (?, ?);
        """
      insertAuthorizedTeacherParams = List(
        SqlParameter("Int", courseGroupID.toString),
        SqlParameter("Int", teacherID.toString)
      )
      _ <- writeDB(insertAuthorizedTeacherSQL, insertAuthorizedTeacherParams)
      _ <- IO(logger.info("[Step 4.1] 成功为课程组添加教师授权"))

      // Step 5: 记录操作日志
      _ <- IO(logger.info("[Step 5] 记录创建课程组的操作日志"))
      operationDetails = s"教师 ${teacherID} 创建了课程组 [${name}]，学分: ${credit}"
      _ <- recordCourseGroupOperationLog(teacherID, "CreateCourseGroup", courseGroupID, operationDetails)

      // Step 6: 构造返回的 CourseGroup 对象
      _ <- IO(logger.info(s"[Step 6] 构造返回的 CourseGroup 对象"))
      createdCourseGroup = CourseGroup(
        courseGroupID = courseGroupID,
        name = name,
        credit = credit,
        ownerTeacherID = teacherID,
        authorizedTeachers = List(teacherID)
      )
      _ <- IO(logger.info(s"[Step 6.1] 创建的课程组信息: ${createdCourseGroup}"))
    } yield createdCourseGroup
  }
}