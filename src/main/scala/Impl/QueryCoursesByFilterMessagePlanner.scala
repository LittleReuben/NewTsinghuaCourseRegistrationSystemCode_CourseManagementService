package Impl

import Objects.UserAccountService.SafeUserInfo
import APIs.UserAuthService.VerifyTokenValidityMessage
import Utils.CourseManagementProcess.fetchCourseByID
import APIs.UserAccountService.QuerySafeUserInfoByUserIDMessage
import Utils.CourseManagementProcess.fetchCourseGroupByID
import Objects.CourseManagementService.CourseGroup
import Objects.CourseManagementService.DayOfWeek
import Objects.CourseManagementService.TimePeriod
import Objects.CourseManagementService.PairOfGroupAndCourse
import Objects.CourseManagementService.CourseTime
import Objects.UserAccountService.UserRole
import Objects.CourseManagementService.CourseInfo
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
import cats.implicits._

case class QueryCoursesByFilterMessagePlanner(
    userToken: String,
    courseGroupID: Option[Int],
    courseGroupName: Option[String],
    teacherName: Option[String],
    allowedTimePeriods: List[CourseTime],
    override val planContext: PlanContext
) extends Planner[List[PairOfGroupAndCourse]] {

  private val logger = LoggerFactory.getLogger(getClass.getSimpleName + "_" + planContext.traceID.id)

  override def plan(using PlanContext): IO[List[PairOfGroupAndCourse]] = {
    for {
      // Step 1: Verify user token validity
      _ <- IO(logger.info(s"验证用户Token: ${userToken}"))
      isTokenValid <- VerifyTokenValidityMessage(userToken).send
      _ <- if (!isTokenValid) IO.raiseError(new IllegalArgumentException("Token验证失败。")) else IO(logger.info("Token验证通过。"))

      // Step 2: Fetch all courses
      _ <- IO(logger.info("从数据表中获取所有课程记录"))
      allCourses <- fetchAllCourses()

      // Step 3: Filter courses based on provided criteria
      _ <- IO(logger.info("根据过滤条件对课程进行筛选"))
      filteredCourses <- filterCourses(allCourses)

      // Fix: Updated structure for filteredCourses, ensuring no compilation error occurs
      coursesWithGroups <- filteredCourses.collect {
        case (course, Some(courseGroup)) =>
          PairOfGroupAndCourse(courseGroup, course)
      }.pure[IO]

      // Step 4: Final output
      _ <- IO(logger.info(s"返回过滤后的课程信息，总计: ${coursesWithGroups.size} 个匹配的课程"))
    } yield coursesWithGroups
  }

  private def fetchAllCourses()(using PlanContext): IO[List[CourseInfo]] = {
    for {
      _ <- IO(logger.info(s"查询所有课程基本信息的SQL语句"))
      
      // 查询所有课程ID
      courseIDRows <- readDBRows(
        s"SELECT course_id FROM ${schemaName}.course_table;",
        List.empty[SqlParameter]
      )
      courseIDs = courseIDRows.map(row => decodeField[Int](row, "course_id"))
      _ <- IO(logger.info(s"找到课程ID总计: ${courseIDs.size}"))

      // 根据课程ID获得完整课程信息
      allCourses <- courseIDs.traverse { courseID =>
        fetchCourseByID(courseID).flatMap {
          case Some(courseInfo) =>
            IO(logger.info(s"成功获取课程信息: ${courseInfo.courseID}"))
              .as(Some(courseInfo))
          case None =>
            IO(logger.warn(s"未找到ID为${courseID}的课程信息")).as(None)
        }
      }.map(_.flatten)
    } yield allCourses
  }

  private def filterCourses(courses: List[CourseInfo])(using PlanContext): IO[List[(CourseInfo, Option[CourseGroup])]] = {
    courses.traverse { course =>
      checkCourseAndGroupValidity(course).map(groupOpt => (course, groupOpt))
    }
  }

  private def checkCourseAndGroupValidity(course: CourseInfo)(using PlanContext): IO[Option[CourseGroup]] = {
    for {
      // Fetch course group and teacher info
      courseGroupOpt <- fetchCourseGroupByID(course.courseGroupID)
      teacherInfoOpt <- QuerySafeUserInfoByUserIDMessage(course.teacherID).send

      // Perform filtering
      isMatching <- IO {
        val isCourseGroupIDMatching = courseGroupID.forall(_ == course.courseGroupID)
        val isCourseGroupNameMatching = courseGroupOpt.exists(cg => courseGroupName.forall(cg.name.contains))
        val isTeacherNameMatching = teacherInfoOpt.exists(ti => teacherName.forall(_ == ti.userName))
        val isTimeMatching = allowedTimePeriods.forall(tp => course.time.exists(_ == tp))

        logger.info(s"[Filtering Course] ID: ${course.courseID}, GroupIDMatch: ${isCourseGroupIDMatching}, GroupNameMatch: ${isCourseGroupNameMatching}, TeacherMatch: ${isTeacherNameMatching}, TimeMatch: ${isTimeMatching}")

        isCourseGroupIDMatching && isCourseGroupNameMatching && isTeacherNameMatching && isTimeMatching
      }
    } yield if (isMatching) courseGroupOpt else None
  }
}
// 模型修复了代码中fetchAllCourses函数的实现，将其修改为逐个查询course_id并通过fetchCourseByID获取完整信息，避免编译错误。