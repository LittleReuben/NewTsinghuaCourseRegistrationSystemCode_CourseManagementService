package Impl


import Common.API.{PlanContext, Planner}
import Common.DBAPI._
import Common.Object.SqlParameter
import Common.ServiceUtils.schemaName
import Objects.CourseManagementService.CourseInfo
import Utils.CourseManagementProcess.validateTeacherToken
import Utils.CourseManagementProcess.fetchCourseByID
import Utils.CourseManagementProcess.recordCourseManagementOperationLog
import Utils.CourseManagementProcess.validateTeacherManagePermission
import cats.effect.IO
import org.slf4j.LoggerFactory
import io.circe._
import io.circe.syntax._
import io.circe.generic.auto._
import org.joda.time.DateTime
import cats.implicits.*
import Common.Serialize.CustomColumnTypes.{decodeDateTime, encodeDateTime}
import APIs.UserAuthService.VerifyTokenValidityMessage
import Objects.CourseManagementService.CourseTime
import Objects.CourseManagementService.DayOfWeek
import Objects.CourseManagementService.TimePeriod
import Objects.UserAccountService.SafeUserInfo
import Objects.UserAccountService.UserRole
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
import Objects.CourseManagementService.CourseInfo
import Common.Serialize.CustomColumnTypes.{decodeDateTime,encodeDateTime}

case class DeleteCourseMessagePlanner(
                                       teacherToken: String,
                                       courseID: Int,
                                       override val planContext: PlanContext
                                     ) extends Planner[String] {
  val logger = LoggerFactory.getLogger(this.getClass.getSimpleName + "_" + planContext.traceID.id)

  override def plan(using planContext: PlanContext): IO[String] = {

    def validateToken(token: String): IO[Int] = for {
      _ <- IO(logger.info(s"开始验证教师 token: ${token}"))
      teacherIDOpt <- validateTeacherToken(token)
      teacherID <- teacherIDOpt match {
        case Some(id) =>
          IO(logger.info(s"token验证成功，教师ID为: ${id}")) >>
            IO.pure(id)
        case None =>
          IO(logger.warn(s"教师 token 验证失败")) >>
            IO.raiseError(new IllegalStateException("教师 token 验证失败"))
      }
    } yield teacherID

    def validateCourseExistence(courseID: Int, teacherID: Int): IO[CourseInfo] = for {
      _ <- IO(logger.info(s"开始根据课程ID ${courseID} 查询课程信息"))
      courseInfoOpt <- fetchCourseByID(courseID)
      courseInfo <- courseInfoOpt match {
        case Some(course) if course.teacherID == teacherID =>
          IO(logger.info(s"课程存在且与当前教师匹配，课程信息: ${course}")) >>
            IO.pure(course)
        case Some(_) =>
          IO(logger.warn(s"课程 ${courseID} 不属于教师 ${teacherID} 或权限不匹配")) >>
            IO.raiseError(new IllegalStateException("课程不属于当前教师或权限不匹配"))
        case None =>
          IO(logger.warn(s"课程ID ${courseID} 不存在")) >>
            IO.raiseError(new IllegalStateException("课程不存在"))
      }
    } yield courseInfo

    def validatePermission(): IO[Unit] = for {
      _ <- IO(logger.info("开始验证当前阶段是否允许删除课程"))
      isAllowed <- validateTeacherManagePermission()
      _ <- if (!isAllowed) {
        IO(logger.warn("当前阶段不允许删除课程")) >>
          IO.raiseError(new IllegalStateException("操作课程权限未开启！"))
      } else IO(logger.info("当前阶段允许删除课程"))
    } yield ()

    def deleteCourse(courseID: Int): IO[String] = for {
      _ <- IO(logger.info(s"准备删除课程ID ${courseID} 的记录"))
      deleteSql =
        s"""
        DELETE FROM ${schemaName}.course_table WHERE course_id = ?
         """.stripMargin
      deleteParams = List(SqlParameter("Int", courseID.toString))
      deleteResult <- writeDB(deleteSql, deleteParams)
      _ <- IO(logger.info(s"课程删除操作结果: ${deleteResult}"))
    } yield deleteResult

    def logOperation(teacherID: Int, courseID: Int): IO[Unit] = for {
      _ <- IO(logger.info(s"开始记录删除课程操作日志，教师ID: ${teacherID}, 课程ID: ${courseID}"))
      logDetails = s"教师ID=${teacherID} 删除了课程ID=${courseID}"
      logResult <- recordCourseManagementOperationLog(teacherID, "删除课程", courseID, logDetails)
      _ <- IO(logger.info(s"操作日志记录结果: ${logResult}"))
    } yield ()

    for {
      teacherID <- validateToken(teacherToken)
      _ <- validatePermission()
      _ <- validateCourseExistence(courseID, teacherID)
      _ <- deleteCourse(courseID)
      _ <- logOperation(teacherID, courseID)
      _ <- IO(logger.info(s"课程ID ${courseID} 删除成功"))
    } yield "删除成功！"
  }
}