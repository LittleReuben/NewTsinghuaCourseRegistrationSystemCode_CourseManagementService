package Impl


import Common.API.{PlanContext, Planner}
import Common.DBAPI._
import Common.Object.SqlParameter
import Common.ServiceUtils.schemaName
import Objects.CourseManagementService.{CourseInfo, CourseTime}
import Objects.CourseManagementService.{DayOfWeek, TimePeriod}
import Objects.UserAccountService.UserRole
import Utils.CourseManagementProcess._
import org.slf4j.LoggerFactory
import cats.effect.IO
import io.circe._
import io.circe.parser._
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
import APIs.UserAuthService.VerifyTokenValidityMessage
import Utils.CourseManagementProcess.fetchCourseByID
import Objects.CourseManagementService.CourseTime
import Utils.CourseManagementProcess.recordCourseManagementOperationLog
import Utils.CourseManagementProcess.checkAuthorizationForTeacher
import Utils.CourseManagementProcess.validateTeacherManagePermission
import Objects.CourseManagementService.DayOfWeek
import Objects.CourseManagementService.TimePeriod
import Utils.CourseManagementProcess.validateCourseTimeConflict
import Objects.UserAccountService.SafeUserInfo
import Utils.CourseManagementProcess.validateTeacherToken
import Objects.CourseManagementService.CourseInfo
import cats.implicits.*
import Common.Serialize.CustomColumnTypes.{decodeDateTime,encodeDateTime}
import Objects.CourseManagementService.CourseInfo

case class CreateCourseMessagePlanner(
  teacherToken: String,
  courseGroupID: Int,
  courseCapacity: Int,
  time: List[CourseTime],
  location: String,
)(override val planContext: PlanContext) extends Planner[CourseInfo] {

  // Logger 使用类路径+TraceID以便调试
  val logger = LoggerFactory.getLogger(this.getClass.getSimpleName + "_" + planContext.traceID.id)

  override def plan(using planContext: PlanContext): IO[CourseInfo] = {
    for {
      // Step 1: Validate teacherToken and fetch teacherID
      _ <- logInfo("[Step 1] Validate teacherToken")
      teacherID <- validateTeacherTokenStep(teacherToken)

      // Step 2: Check if teacher is authorized for the course group
      _ <- logInfo("[Step 2] Validate teacher's authorization for the course group")
      _ <- validateTeacherAuthorizationStep(teacherID, courseGroupID)

      // Step 3: Check if current phase allows course creation
      _ <- logInfo("[Step 3] Verify if course creation is allowed in the current phase")
      _ <- validateManagePermissionStep()

      // Step 4: Validate course time conflict
      _ <- logInfo("[Step 4] Validate that there is no timing conflict")
      _ <- validateTimeConflictStep(teacherID, time)

      // Step 5: Insert the new course record into the database
      _ <- logInfo("[Step 5] Insert new course record into the database")
      courseID <- insertCourseRecordStep(teacherID, courseGroupID, courseCapacity, time, location)

      // Step 6: Record operation log for course creation
      _ <- logInfo("[Step 6] Record course creation operation log")
      _ <- recordOperationLogStep(teacherID, courseID, courseCapacity, time, location)

      // Step 7: Fetch and return the newly created course info
      _ <- logInfo("[Step 7] Fetch the newly created course info and return")
      courseInfo <- constructCourseInfoStep(courseID, teacherID, courseGroupID, courseCapacity, time, location)
    } yield courseInfo
  }

  private def logInfo(message: String)(using PlanContext): IO[Unit] =
    IO(logger.info(message))

  private def validateTeacherTokenStep(teacherToken: String)(using PlanContext): IO[Int] = {
    for {
      optionalResult <- validateTeacherToken(teacherToken)
      teacherID <- optionalResult match {
        case Some(id) =>
          IO(logger.info(s"教师 Token 验证通过，教师ID: ${id}")) >>
          IO.pure(id)
        case None =>
          val errorMsg = s"教师鉴权失败，传入的教师 Token 为 ${teacherToken}"
          logger.error(errorMsg)
          IO.raiseError(new IllegalArgumentException(errorMsg))
      }
    } yield teacherID
  }

  private def validateTeacherAuthorizationStep(teacherID: Int, courseGroupID: Int)(using PlanContext): IO[Unit] = {
    for {
      isAuthorized <- checkAuthorizationForTeacher(teacherID, courseGroupID)
      _ <- if (!isAuthorized) {
        val errorMsg = s"未获得课程组 [ID: ${courseGroupID}] 的授权"
        logger.error(errorMsg)
        IO.raiseError(new IllegalStateException(errorMsg))
      } else IO(logger.info(s"教师 [ID: ${teacherID}] 被授权开设课程组 [ID: ${courseGroupID}]"))
    } yield ()
  }

  private def validateManagePermissionStep()(using PlanContext): IO[Unit] = {
    for {
      canCreate <- validateTeacherManagePermission()
      _ <- if (!canCreate) {
        val errorMsg = "当前阶段不允许创建课程"
        logger.error(errorMsg)
        IO.raiseError(new IllegalStateException(errorMsg))
      } else IO(logger.info("当前阶段允许创建课程"))
    } yield ()
  }

  private def validateTimeConflictStep(teacherID: Int, time: List[CourseTime])(using PlanContext): IO[Unit] = {
    for {
      hasConflict <- validateCourseTimeConflict(teacherID, time)
      _ <- if (hasConflict) {
        val errorMsg = "时间冲突，无法创建课程"
        logger.error(errorMsg)
        IO.raiseError(new IllegalStateException(errorMsg))
      } else IO(logger.info("时间冲突检查通过，无冲突"))
    } yield ()
  }

  private def insertCourseRecordStep(
    teacherID: Int,
    courseGroupID: Int,
    courseCapacity: Int,
    time: List[CourseTime],
    location: String
  )(using PlanContext): IO[Int] = {
    val sql =
      s"""
         |INSERT INTO ${schemaName}.course_table (course_group_id, course_capacity, time, location, teacher_id)
         |VALUES (?, ?, ?, ?, ?)
         |RETURNING course_id;
       """.stripMargin

    val parameters = List(
      SqlParameter("Int", courseGroupID.toString),
      SqlParameter("Int", courseCapacity.toString),
      SqlParameter("String", time.asJson.noSpaces),
      SqlParameter("String", location),
      SqlParameter("Int", teacherID.toString)
    )

    for {
      courseID <- readDBInt(sql, parameters)
      _ <- IO(logger.info(s"新课程记录插入完成，生成的课程ID为: ${courseID}"))
    } yield courseID
  }

  private def recordOperationLogStep(
    teacherID: Int,
    courseID: Int,
    courseCapacity: Int,
    time: List[CourseTime],
    location: String
  )(using PlanContext): IO[Unit] = {
    val details = s"创建了课程：容量=${courseCapacity}, 时间=${time.asJson.noSpaces}, 地点=${location}"
    recordCourseManagementOperationLog(teacherID, "CreateCourse", courseID, details)
  }

  private def constructCourseInfoStep(
    courseID: Int,
    teacherID: Int,
    courseGroupID: Int,
    courseCapacity: Int,
    time: List[CourseTime],
    location: String
  ): IO[CourseInfo] = {
    IO {
      CourseInfo(
        courseID = courseID,
        courseCapacity = courseCapacity,
        time = time,
        location = location,
        courseGroupID = courseGroupID,
        teacherID = teacherID,
        preselectedStudentsSize = 0,
        selectedStudentsSize = 0,
        waitingListSize = 0
      )
    }.flatTap(course => IO(logger.info(s"新增课程信息: ${course}")))
  }
}