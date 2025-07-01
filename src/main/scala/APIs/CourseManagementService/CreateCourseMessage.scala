package APIs.CourseManagementService

import Common.API.API
import Global.ServiceCenter.CourseManagementServiceCode

import io.circe.{Decoder, Encoder, Json}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.*
import io.circe.parser.*
import Common.Serialize.CustomColumnTypes.{decodeDateTime, encodeDateTime}

import com.fasterxml.jackson.core.`type`.TypeReference
import Common.Serialize.JacksonSerializeUtils

import scala.util.Try

import org.joda.time.DateTime
import java.util.UUID
import Objects.CourseManagementService.{CourseTime, CourseInfo}
import Common.API.{PlanContext, Planner}
import Common.DBAPI._
import Common.Object.ParameterList
import Common.Object.SqlParameter
import Common.ServiceUtils.schemaName
import cats.effect.IO
import org.slf4j.LoggerFactory

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
) extends API[CourseInfo](CourseManagementServiceCode)

object CreateCourseMessage {

  import Common.Serialize.CustomColumnTypes.{decodeDateTime, encodeDateTime}

  // Circe 默认的 Encoder 和 Decoder
  private val circeEncoder: Encoder[CreateCourseMessage] = deriveEncoder
  private val circeDecoder: Decoder[CreateCourseMessage] = deriveDecoder

  // Jackson 对应的 Encoder 和 Decoder
  private val jacksonEncoder: Encoder[CreateCourseMessage] = Encoder.instance { currentObj =>
    Json.fromString(JacksonSerializeUtils.serialize(currentObj))
  }

  private val jacksonDecoder: Decoder[CreateCourseMessage] = Decoder.instance { cursor =>
    try { Right(JacksonSerializeUtils.deserialize(cursor.value.noSpaces, new TypeReference[CreateCourseMessage]() {})) } 
    catch { case e: Throwable => Left(io.circe.DecodingFailure(e.getMessage, cursor.history)) }
  }

  // Circe + Jackson 兜底的 Encoder
  given createCourseMessageEncoder: Encoder[CreateCourseMessage] = Encoder.instance { config =>
    Try(circeEncoder(config)).getOrElse(jacksonEncoder(config))
  }

  // Circe + Jackson 兜底的 Decoder
  given createCourseMessageDecoder: Decoder[CreateCourseMessage] = Decoder.instance { cursor =>
    circeDecoder.tryDecode(cursor).orElse(jacksonDecoder.tryDecode(cursor))
  }

  def executeCreateCourseMessage(message: CreateCourseMessage)(using PlanContext): IO[CourseInfo] = {
    val logger = LoggerFactory.getLogger(this.getClass)

    for {
      _ <- IO(logger.info(s"Start creating course for teacher with token: ${message.teacherToken}"))

      // Validate teacher's token, ensure it exists and is valid
      teacherID <- readDBInt(
        s"SELECT teacher_id FROM ${schemaName}.teachers WHERE token = ?;",
        List(SqlParameter("String", message.teacherToken))
      ).recoverWith {
        case e: Throwable =>
          IO.raiseError(new IllegalArgumentException(s"Invalid teacher token: ${message.teacherToken}"))
      }

      // Insert new course into the courses table
      currentTime = DateTime.now()
      courseID <- writeDB(
        s"""
           INSERT INTO ${schemaName}.courses 
           (course_group_id, course_capacity, location, created_at, updated_at, teacher_id) 
           VALUES (?, ?, ?, ?, ?, ?) RETURNING course_id;
         """,
        List(
          SqlParameter("Int", message.courseGroupID.toString),
          SqlParameter("Int", message.courseCapacity.toString),
          SqlParameter("String", message.location),
          SqlParameter("DateTime", currentTime.getMillis.toString),
          SqlParameter("DateTime", currentTime.getMillis.toString),
          SqlParameter("Int", teacherID.toString)
        )
      ).map(result => decodeType[Int](result)).recoverWith {
        case e: Throwable =>
          IO.raiseError(new IllegalStateException(s"Failed to create course. Reason: ${e.getMessage}"))
      }

      // Insert course times into the course_times table
      courseTimesParams = message.time.map { courseTime =>
        ParameterList(
          List(
            SqlParameter("Int", courseID.toString),
            SqlParameter("String", courseTime.dayOfWeek.toString),
            SqlParameter("String", courseTime.timePeriod.toString)
          )
        )
      }
      _ <- writeDBList(
        s"""
           INSERT INTO ${schemaName}.course_times 
           (course_id, day_of_week, time_period) 
           VALUES (?, ?, ?);
         """,
        courseTimesParams
      ).recoverWith {
        case e: Throwable =>
          IO.raiseError(new IllegalStateException(s"Failed to insert course times. Reason: ${e.getMessage}"))
      }

      // Fetch created course info
      courseInfoJson <- readDBJson(
        s"""
           SELECT c.course_id, c.course_capacity, c.location, c.course_group_id, c.teacher_id, 
                  COALESCE(c.selected_students_size, 0) AS selected_students_size, 
                  COALESCE(c.waiting_list_size, 0) AS waiting_list_size, 
                  COALESCE(c.preselected_students_size, 0) AS preselected_students_size 
           FROM ${schemaName}.courses c 
           WHERE c.course_id = ?;
         """,
        List(SqlParameter("Int", courseID.toString))
      )
      courseInfo = decodeType[CourseInfo](courseInfoJson)

    } yield courseInfo
  }

}