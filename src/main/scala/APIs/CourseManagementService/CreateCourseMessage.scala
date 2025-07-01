package APIs.CourseManagementService

import Common.API.API
import Global.ServiceCenter.CourseManagementServiceCode

import io.circe.{Decoder, Encoder, Json}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.throwError
import io.circe.parser.decode
import Common.Serialize.CustomColumnTypes.{decodeDateTime, encodeDateTime}
import Objects.CourseManagementService.{CourseTime, CourseInfo}
import Objects.CourseManagementService.DayOfWeek
import Objects.CourseManagementService.TimePeriod
import Common.API.{PlanContext, Planner}
import Common.DBAPI._
import Common.Object.ParameterList
import Common.Object.SqlParameter
import Common.ServiceUtils.schemaName
import cats.effect.IO
import org.slf4j.LoggerFactory
import org.joda.time.DateTime
import java.util.UUID

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
    try {
      Right(JacksonSerializeUtils.deserialize(cursor.value.noSpaces, new com.fasterxml.jackson.core.`type`.TypeReference[CreateCourseMessage]() {}))
    } catch {
      case e: Throwable => Left(io.circe.DecodingFailure(e.getMessage, cursor.history))
    }
  }

  // Circe + Jackson 兜底的 Encoder
  given createCourseMessageEncoder: Encoder[CreateCourseMessage] = Encoder.instance { config =>
    Try(circeEncoder(config)).getOrElse(jacksonEncoder(config))
  }

  // Circe + Jackson 兜底的 Decoder
  given createCourseMessageDecoder: Decoder[CreateCourseMessage] = Decoder.instance { cursor =>
    circeDecoder.tryDecode(cursor).orElse(jacksonDecoder.tryDecode(cursor))
  }

  /**
   * Database query and insertion logic for creating new courses
   */
  def createCourse(createMessage: CreateCourseMessage)(using planContext: PlanContext): IO[CourseInfo] = {
    val logger = LoggerFactory.getLogger(this.getClass)

    for {
      _ <- IO(logger.info(s"[Step 1] Validating teacher token: ${createMessage.teacherToken}"))

      // Validate Teacher Token -> Assume teacherID is derived here (requires supplementary API)
      teacherID <- validateTeacherToken(createMessage.teacherToken)

      _ <- IO(logger.info(s"[Step 2] Inserting new course into database"))

      // Step 2.1: Insert course information into database
      courseID <- insertCourse(
        teacherID,
        createMessage.courseGroupID,
        createMessage.courseCapacity,
        createMessage.location,
        createMessage.time
      )

      _ <- IO(logger.info(s"[Step 3] Successfully created course with ID: $courseID"))

      // Step 3: Retrieve created course details from database
      createdCourse <- retrieveCourseByID(courseID)
    } yield createdCourse
  }

  private def validateTeacherToken(token: String)(using planContext: PlanContext): IO[Int] = {
    // Here you need to validate the token and return the teacherID. This might require another API.
    val sqlQuery =
      s"""
         |SELECT teacher_id
         |FROM ${schemaName}.teacher_token
         |WHERE token_value = ? AND is_valid = true;
         |""".stripMargin

    readDBInt(sqlQuery, List(SqlParameter("String", token)))
  }

  private def insertCourse(
    teacherID: Int,
    courseGroupID: Int,
    courseCapacity: Int,
    location: String,
    time: List[CourseTime]
  )(using planContext: PlanContext): IO[Int] = {
    val sqlQuery =
      s"""
         |INSERT INTO ${schemaName}.courses
         |(teacher_id, course_group_id, course_capacity, location, course_time)
         |VALUES (?, ?, ?, ?, ?)
         |RETURNING course_id;
         |""".stripMargin

    readDBInt(
      sqlQuery,
      List(
        SqlParameter("Int", teacherID.toString),
        SqlParameter("Int", courseGroupID.toString),
        SqlParameter("Int", courseCapacity.toString),
        SqlParameter("String", location),
        SqlParameter("String", time.asJson.noSpaces)
      )
    )
  }

  private def retrieveCourseByID(courseID: Int)(using planContext: PlanContext): IO[CourseInfo] = {
    val sqlQuery =
      s"""
         |SELECT *
         |FROM ${schemaName}.courses
         |WHERE course_id = ?;
         |""".stripMargin

    readDBJson(sqlQuery, List(SqlParameter("Int", courseID.toString))).map(decodeType[CourseInfo])
  }
}