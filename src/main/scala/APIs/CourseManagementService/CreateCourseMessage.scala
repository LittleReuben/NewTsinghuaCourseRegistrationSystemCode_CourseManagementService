package APIs.CourseManagementService

import Common.API.PlanContext
import Common.DBAPI._
import Common.Object.SqlParameter
import Common.ServiceUtils.schemaName
import Objects.CourseManagementService.{CourseTime, CourseInfo}
import Common.Serialize.CustomColumnTypes.{decodeDateTime, encodeDateTime}

import cats.effect.IO
import org.joda.time.DateTime

/**
 * CreateCourseMessage
 * desc: 用于处理创建课程的功能需求。
 * @param teacherToken: String (教师的身份验证令牌，用于鉴权。)
 * @param courseGroupID: Int (课程组ID，用于标识所属课程组。)
 * @param courseCapacity: Int (课程容量，表示该课程允许的最大学生人数。)
 * @param time: List[CourseTime] (课程上课时间列表，包含每节课的时间信息。)
 * @param location: String (课程的上课地点。)
 * @return createdCourse: CourseInfo (新创建的课程信息，包括课程ID、课程容量、时间、地点、课程组信息等。)
 */

case class CreateCourseMessage(
  teacherToken: String,
  courseGroupID: Int,
  courseCapacity: Int,
  time: List[CourseTime],
  location: String
) {
  def create()(using PlanContext): IO[CourseInfo] = {
    for {
      // Step 1: 验证教师身份
      teacherID <- validateTeacherToken(teacherToken)
      
      // Step 2: 写入课程信息至数据库
      courseID <- writeCourseInfo(courseGroupID, courseCapacity, time, location, teacherID)
      
      // Step 3: 构建返回的课程信息
      courseInfo <- buildCourseInfo(courseID, courseGroupID, courseCapacity, time, location, teacherID)
    } yield courseInfo
  }

  private def validateTeacherToken(token: String)(using PlanContext): IO[Int] = {
    val sql = s"SELECT teacher_id FROM ${schemaName}.teacher_tokens WHERE token_value = ? AND is_valid = true;"
    readDBInt(sql, List(SqlParameter("String", token)))
  }

  private def writeCourseInfo(
    courseGroupID: Int, 
    courseCapacity: Int, 
    time: List[CourseTime], 
    location: String, 
    teacherID: Int
  )(using PlanContext): IO[Int] = {
    val sql =
      s"""
         INSERT INTO ${schemaName}.courses (course_group_id, capacity, time, location, teacher_id)
         VALUES (?, ?, ?, ?, ?)
         RETURNING course_id;
       """
    val params = List(
      SqlParameter("Int", courseGroupID.toString),
      SqlParameter("Int", courseCapacity.toString),
      SqlParameter("String", time.asJson.noSpaces),
      SqlParameter("String", location),
      SqlParameter("Int", teacherID.toString)
    )
    readDBInt(sql, params)
  }

  private def buildCourseInfo(
    courseID: Int,
    courseGroupID: Int,
    courseCapacity: Int,
    time: List[CourseTime],
    location: String,
    teacherID: Int
  )(using PlanContext): IO[CourseInfo] = {
    // 构建CourseInfo对象
    val sql =
      s"""
         SELECT 
           COALESCE(preselected_students_size, 0) AS preselected_students_size,
           COALESCE(selected_students_size, 0) AS selected_students_size,
           COALESCE(waiting_list_size, 0) AS waiting_list_size
         FROM ${schemaName}.courses
         WHERE course_id = ?;
       """
    readDBJson(sql, List(SqlParameter("Int", courseID.toString))).map { json =>
      CourseInfo(
        courseID = courseID,
        courseCapacity = courseCapacity,
        time = time,
        location = location,
        courseGroupID = courseGroupID,
        teacherID = teacherID,
        preselectedStudentsSize = decodeField[Int](json, "preselected_students_size"),
        selectedStudentsSize = decodeField[Int](json, "selected_students_size"),
        waitingListSize = decodeField[Int](json, "waiting_list_size")
      )
    }
  }

}