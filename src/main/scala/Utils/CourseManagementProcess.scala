package Utils

//process plan import 预留标志位，不要删除
import io.circe._
import io.circe.syntax._
import io.circe.generic.auto._
import org.joda.time.DateTime
import Common.DBAPI._
import Common.ServiceUtils.schemaName
import org.slf4j.LoggerFactory
import APIs.UserAuthService.VerifyTokenValidityMessage
import APIs.UserAccountService.QuerySafeUserInfoByTokenMessage
import Objects.UserAccountService.UserRole
import Objects.UserAccountService.SafeUserInfo
import Common.API.{PlanContext, Planner}
import Common.Object.SqlParameter
import cats.effect.IO
import cats.implicits.*
import Common.Serialize.CustomColumnTypes.{decodeDateTime, encodeDateTime}
import Common.Serialize.CustomColumnTypes.{decodeDateTime,encodeDateTime}
import Objects.CourseManagementService.CourseTime
import Objects.CourseManagementService.DayOfWeek
import Objects.CourseManagementService.TimePeriod
import Common.API.PlanContext
import Objects.CourseManagementService.CourseGroup
import Utils.CourseManagementProcess.fetchCourseByID
import Objects.CourseManagementService.CourseInfo
import Utils.CourseManagementProcess.fetchCourseGroupByID
import Objects.SystemLogService.SystemLogEntry
import Utils.CourseManagementProcess.validateTeacherToken
import cats.implicits._
import io.circe.Json
import io.circe.parser._

case object CourseManagementProcess {
  private val logger = LoggerFactory.getLogger(getClass)
  //process plan code 预留标志位，不要删除
  
  def validateTeacherToken(teacherToken: String)(using PlanContext): IO[Option[Int]] = {
  // val logger = LoggerFactory.getLogger("validateTeacherToken")  // 同文后端处理: logger 统一
    
    for {
      // Step 1: 验证 token 的有效性
      _ <- IO(logger.info(s"开始验证传入的教师 token: ${teacherToken}"))
      isTokenValid <- VerifyTokenValidityMessage(teacherToken).send
  
      _ <- IO {
        if (!isTokenValid) logger.warn(s"教师 token 无效或已过期: ${teacherToken}")
      }
  
      // 如果 token 无效，直接返回 None
      result <- if (!isTokenValid) IO.pure(None)
                else {
                  for {
                    // Step 2: 通过 token 查询用户账户的详细信息
                    _ <- IO(logger.info(s"验证有效 token 开始获取用户信息: ${teacherToken}"))
                    userInfoOption <- QuerySafeUserInfoByTokenMessage(teacherToken).send
  
                    _ <- IO(logger.info(s"用户信息查询结果: ${userInfoOption}"))
                    
                    // 如果未找到用户信息，返回 None
                    result <- userInfoOption match {
                      case None =>
                        for {
                          _ <- IO(logger.warn(s"通过 token 获取不到任何用户信息: ${teacherToken}"))
                        } yield None
  
                      case Some(userInfo) =>
                        for {
                          _ <- IO(logger.info(s"解析用户信息, ID: ${userInfo.userID}, 角色: ${userInfo.role}"))
  
                          // 如果角色不是 Teacher，返回 None
                          teacherID <- if (userInfo.role != UserRole.Teacher) {
                            IO(logger.warn(s"用户角色不是教师: ${userInfo.role}, token: ${teacherToken}")) >>
                            IO.pure(None)
                          } else {
                            IO(logger.info(s"用户角色是教师, ID 为: ${userInfo.userID}")) >>
                            IO.pure(Some(userInfo.userID))
                          }
                        } yield teacherID
                    }
                  } yield result
                }
    } yield result
  }
  
  def validateCourseTimeConflict(teacherID: Int, courseTimeToCheck: List[CourseTime])(using PlanContext): IO[Boolean] = {
    logger.info(s"开始检查老师ID=${teacherID}的新增课程时间是否存在冲突")
  
    val sql = s"SELECT time FROM ${schemaName}.course_table WHERE teacher_id = ?"
    val parameters = List(SqlParameter("Int", teacherID.toString))
  
    for {
      // Step 1: 查询老师已开设课程时间安排
      _ <- IO(logger.info(s"执行 SQL 查询：${sql}"))
      rows <- readDBRows(sql, parameters)
  
      // Step 1.1: 解析字段time为CourseTime对象列表
      existingCourseTimes <- IO {
        rows.map { row =>
          val timeString = decodeField[String](row, "time")
          decodeType[CourseTime](timeString)
        }
      }
      _ <- IO(logger.info(s"已开设课程时间列表解析完成，共 ${existingCourseTimes.size} 条记录"))
  
      // Step 2: 逐项比较时间安排
      isConflict <- IO {
        courseTimeToCheck.exists { newCourseTime =>
          logger.debug(s"检查新增课程时间：dayOfWeek=${newCourseTime.dayOfWeek}, timePeriod=${newCourseTime.timePeriod}")
          existingCourseTimes.exists { existingTime =>
            existingTime.dayOfWeek == newCourseTime.dayOfWeek &&
            existingTime.timePeriod == newCourseTime.timePeriod
          }
        }
      }
  
      _ <- IO(
        if (isConflict) {
          logger.info("时间冲突验证发现冲突，返回 true")
        } else {
          logger.info("时间冲突验证通过，无冲突，返回 false")
        }
      )
    } yield isConflict
  }
  
  def fetchCourseGroupByID(courseGroupID: Int)(using PlanContext): IO[Option[CourseGroup]] = {
  // val logger = LoggerFactory.getLogger(getClass)  // 同文后端处理: logger 统一
  
    logger.info(s"开始查询课程组信息，courseGroupID=${courseGroupID}")
  
    val courseGroupSql = s"""
         SELECT course_group_id, name, credit, owner_teacher_id
         FROM ${schemaName}.course_group_table
         WHERE course_group_id = ?
       """
    val authorizedTeachersSql = s"""
         SELECT authorized_teacher_id
         FROM ${schemaName}.authorized_teachers_table
         WHERE course_group_id = ?
       """
  
    for {
      _ <- IO(logger.info(s"[Step 1] 查询课程组基本信息 SQL: ${courseGroupSql}"))
      courseGroupOpt <- readDBJsonOptional(courseGroupSql, List(SqlParameter("Int", courseGroupID.toString)))
      _ <- IO(logger.info(s"[Step 2] 查询课程组基本信息完成：${courseGroupOpt.map(_.noSpaces).getOrElse("无匹配记录")}"))
  
      _ <- IO(logger.info(s"[Step 3] 检查课程组信息是否存在"))
      resultOpt <- courseGroupOpt match {
        case None =>
          IO(logger.info(s"[Step 3.1] courseGroupID=${courseGroupID} 不存在课程组信息，返回 None")).map(_ => None)
        case Some(courseGroupJson) =>
          for {
            courseGroupIDDecoded <- IO(decodeField[Int](courseGroupJson, "course_group_id"))
            nameDecoded <- IO(decodeField[String](courseGroupJson, "name"))
            creditDecoded <- IO(decodeField[Int](courseGroupJson, "credit"))
            ownerTeacherIDDecoded <- IO(decodeField[Int](courseGroupJson, "owner_teacher_id"))
  
            _ <- IO(logger.info(s"[Step 4] 查询授权教师列表 SQL: ${authorizedTeachersSql}"))
            authorizedTeachersRows <- readDBRows(authorizedTeachersSql, List(SqlParameter("Int", courseGroupIDDecoded.toString)))
            authorizedTeachers <- IO {
              authorizedTeachersRows.map(json => decodeField[Int](json, "authorized_teacher_id"))
            }
            _ <- IO(logger.info(s"[Step 5] 授权教师查询完成：${authorizedTeachers.mkString(", ")}"))
  
            courseGroup = CourseGroup(
              courseGroupID = courseGroupIDDecoded,
              name = nameDecoded,
              credit = creditDecoded,
              ownerTeacherID = ownerTeacherIDDecoded,
              authorizedTeachers = authorizedTeachers
            )
  
            _ <- IO(logger.info(s"[Step 6] 构建 CourseGroup 对象完成：${courseGroup}"))
          } yield Some(courseGroup)
      }
    } yield resultOpt
  }
  
  def recordCourseManagementOperationLog(
    teacherID: Int,
    operation: String,
    courseID: Int,
    details: String
  )(using PlanContext): IO[String] = {
  // val logger = LoggerFactory.getLogger("CourseManagementLogger")  // 同文后端处理: logger 统一
    
    // 开始日志记录
    logger.info(s"[recordCourseManagementOperationLog] 开始处理教师ID=${teacherID}, 课程ID=${courseID}, 操作=${operation}, 详细信息=${details}")
  
    // Step 1: 输入参数合法性检查
    if (teacherID <= 0 || courseID <= 0) {
      IO.raiseError(new IllegalArgumentException(s"无效的教师ID或课程ID，教师ID=${teacherID}, 课程ID=${courseID}"))
    } else if (operation.isBlank || details.isBlank) {
      IO.raiseError(new IllegalArgumentException(s"操作信息或详细说明不能为空，操作=${operation}, 详细信息=${details}"))
    } else {
      logger.info(s"[recordCourseManagementOperationLog] 参数校验通过")
  
      for {
        // Step 2.1 查询目标课程信息
        fetchedCourseOption <- fetchCourseByID(courseID)
        _ <- IO(logger.info(s"[recordCourseManagementOperationLog] 查询目标课程结果=${fetchedCourseOption}"))
  
        // Step 2.2 检查课程是否存在
        _ <- fetchedCourseOption match {
          case None =>
            val errorMsg = s"课程不存在，课程ID=${courseID}"
            logger.error(s"[recordCourseManagementOperationLog] ${errorMsg}")
            IO.raiseError(new IllegalStateException(errorMsg))
          case Some(_) =>
            IO(logger.info(s"[recordCourseManagementOperationLog] 课程存在，继续处理日志记录"))
        }
  
        // Step 2.3 准备日志记录相关信息
        timestamp <- IO(DateTime.now().getMillis.toString)
        sql <- IO {
          s"""
          INSERT INTO ${schemaName}.operation_log (teacher_id, course_id, action, details, timestamp)
          VALUES (?, ?, ?, ?, ?)
          """
        }
        parameters <- IO {
          List(
            SqlParameter("Int", teacherID.toString),
            SqlParameter("Int", courseID.toString),
            SqlParameter("String", operation),
            SqlParameter("String", details),
            SqlParameter("DateTime", timestamp)
          )
        }
        _ <- IO(logger.info(s"[recordCourseManagementOperationLog] 准备写入日志 SQL=${sql}，参数包含教师ID=${teacherID}, 课程ID=${courseID}, 操作=${operation}, 时间戳=${timestamp}"))
  
        // Step 2.4 写入日志到数据库
        writeResult <- writeDB(sql, parameters)
        _ <- IO(logger.info(s"[recordCourseManagementOperationLog] 写入数据库结果=${writeResult}"))
  
        // Step 3: 返回记录成功的确认信息
      } yield s"日志记录成功：教师ID=${teacherID}, 课程ID=${courseID}, 操作=${operation}, 时间戳=${timestamp}"
    }
  }
  
  
  def checkAuthorizationForTeacher(teacherID: Int, courseGroupID: Int)(using PlanContext): IO[Boolean] = {
  // val logger = LoggerFactory.getLogger("AuthorizationLogger")  // 同文后端处理: logger 统一
    
    for {
      _ <- IO(logger.info(s"[Authorization Check] 开始检查教师ID: ${teacherID} 是否被授权开设课程组ID: ${courseGroupID}"))
  
      // Construct SQL query to check authorization
      sqlQuery <- IO {
        s"""
           SELECT 1
           FROM ${schemaName}.authorized_teachers_table
           WHERE authorized_teacher_id = ? AND course_group_id = ?
         """.stripMargin
      }
  
      // Prepare query parameters
      queryParameters <- IO {
        List(
          SqlParameter("Int", teacherID.toString),
          SqlParameter("Int", courseGroupID.toString)
        )
      }
  
      // Execute query to check if authorization record exists
      _ <- IO(logger.info(s"[Authorization Check] 执行查询授权记录，SQL: ${sqlQuery}, 参数: ${queryParameters.map(_.value).mkString(", ")}"))
      authorizationResult <- readDBJsonOptional(sqlQuery, queryParameters)
  
      // Determine authorization status
      isAuthorized <- IO {
        authorizationResult.isDefined
      }
  
      // Log the result
      _ <- IO(logger.info(s"[Authorization Check] 查询结果: ${if (isAuthorized) "通过授权验证" else "未授权"}"))
    } yield isAuthorized
  }
  
  def recordCourseGroupOperationLog(
      teacherID: Int,
      operation: String,
      courseGroupID: Int,
      details: String
  )(using PlanContext): IO[Unit] = {
  // val logger = LoggerFactory.getLogger(getClass)  // 同文后端处理: logger 统一
  
    for {
      // Step 1: Validate teacherID
      _ <- IO(logger.info(s"开始验证教师ID和课程组ID的有效性: teacherID=${teacherID}, courseGroupID=${courseGroupID}"))
      teacherIDOpt <- validateTeacherToken(teacherID.toString)
      validatedTeacherID <- teacherIDOpt match {
        case Some(id) => IO(logger.info(s"教师ID验证通过: teacherID=${id}")).as(id)
        case None => 
          IO(logger.error(s"教师ID验证失败: teacherID=${teacherID}")) >>
          IO.raiseError(new IllegalArgumentException(s"教师ID[${teacherID}]验证失败"))
      }
  
      // Step 2: Validate courseGroupID
      courseGroupOpt <- fetchCourseGroupByID(courseGroupID)
      validatedCourseGroup <- courseGroupOpt match {
        case Some(group) => 
          IO(logger.info(s"课程组ID验证通过: courseGroupID=${courseGroupID}, courseGroup=${group}")).as(group)
        case None => 
          IO(logger.error(s"课程组ID验证失败: courseGroupID=${courseGroupID}")) >>
          IO.raiseError(new IllegalArgumentException(s"课程组ID[${courseGroupID}]不存在"))
      }
  
      // Step 3: Construct log entry
      timestamp <- IO { DateTime.now() }
      systemLogEntry <- IO {
        SystemLogEntry(
          logID = 0, // 假定数据库自动生成logID
          timestamp = timestamp,
          userID = validatedTeacherID,
          action = operation,
          details = details
        )
      }
      _ <- IO(logger.info(s"准备写入操作日志: systemLogEntry=${systemLogEntry}"))
  
      // Step 4: Write log entry to database
      writeSQL <- IO {
        s"""
        INSERT INTO ${schemaName}.system_log_table 
        (timestamp, user_id, action, details)
        VALUES (?, ?, ?, ?)
        """
      }
      writeParams <- IO {
        List(
          SqlParameter("DateTime", timestamp.getMillis.toString),
          SqlParameter("Int", validatedTeacherID.toString),
          SqlParameter("String", operation),
          SqlParameter("String", details)
        )
      }
      writeResult <- writeDB(writeSQL, writeParams)
      _ <- IO(logger.info(s"日志记录完成，数据库返回: ${writeResult}"))
  
    } yield ()
  }
  
  /**
   * 验证教师管理权限的方法
   * @param PlanContext: 使用隐式参数传递上下文。
   * @return 一个布尔值，表示是否允许教师进行管理权限操作。
   */
  def validateTeacherManagePermission()(using PlanContext): IO[Boolean] = {
  // val logger = LoggerFactory.getLogger("validateTeacherManagePermission")  // 同文后端处理: logger 统一
  
    for {
      // Step 1: 调用 QuerySemesterPhaseStatus 方法获取当前学期阶段信息及其权限
      _ <- IO(logger.info("[validateTeacherManagePermission] 调用 QuerySemesterPhaseStatus 方法获取当前学期阶段权限信息"))
  
      // 替换旧代码，正确调用查询方法（QuerySemesterPhaseStatusMessage 不存在）
      permissionsJson <- readDBJson(
        s"SELECT permissions FROM ${schemaName}.semester_phase_status WHERE current_phase = true;",
        List()
      )
  
      // Step 2: 解析 Permissions 对象中的 allowTeacherManage 字段
      allowTeacherManage <- IO {
        decodeField[Boolean](permissionsJson, "allow_teacher_manage")
      }
  
      // Step 3: 返回 allowTeacherManage 值
      _ <- IO(logger.info(s"[validateTeacherManagePermission] allowTeacherManage 字段值: ${allowTeacherManage}"))
    } yield allowTeacherManage
  }
  
  // Reason for fixing: The original code attempted to call an undefined `QuerySemesterPhaseStatusMessage`. This has been resolved by replacing it with a database query (`readDBJson`) to fetch the necessary data regarding `permissions` from the semester phase status table.
  
  def fetchCourseByID(courseID: Int)(using PlanContext): IO[Option[CourseInfo]] = {
    logger.info(s"开始查询课程信息，传入的课程ID为 ${courseID}。")
  
    // 检查课程ID是否有效（非负）
    if (courseID <= 0) {
      logger.warn(s"课程ID ${courseID} 无效，直接返回 None。")
      IO(None)
    } else {
      val sqlQuery =
        s"""
SELECT course_id, course_capacity, time, location, course_group_id, teacher_id
FROM ${schemaName}.course_table
WHERE course_id = ?;
           """.stripMargin
  
      logger.info(s"数据库查询SQL生成完成：${sqlQuery}")
  
      val queryParameters = IO(List(SqlParameter("Int", courseID.toString)))
  
      for {
        parameters <- queryParameters
        // 执行查询，尝试获取数据库返回值
        optionalCourseJson <- readDBJsonOptional(sqlQuery, parameters)
        _ <- IO(logger.info(s"数据库返回结果是否存在：${optionalCourseJson.isDefined}"))
  
        // 如果查询结果存在，则解析数据为 CourseInfo
        courseInfo <- IO {
          optionalCourseJson.map { courseJson =>
            logger.info(s"开始解析数据库返回值为 CourseInfo 对象，返回 JSON 为：${courseJson}")
  
            val courseIDValue = decodeField[Int](courseJson, "course_id")
            val courseCapacityValue = decodeField[Int](courseJson, "course_capacity")
            val timeRaw = decodeField[String](courseJson, "time")
            val timeParsed = parse(timeRaw).getOrElse(Json.Null).as[List[Json]].getOrElse(List.empty).map { timeJson =>
              logger.info(s"解析课程时间字段：${timeJson}")
              val dayOfWeekValue = DayOfWeek.fromString(decodeField[String](timeJson, "day_of_week"))
              val timePeriodValue = TimePeriod.fromString(decodeField[String](timeJson, "time_period"))
              CourseTime(dayOfWeekValue, timePeriodValue)
            }
            val locationValue = decodeField[String](courseJson, "location")
            val courseGroupIDValue = decodeField[Int](courseJson, "course_group_id")
            val teacherIDValue = decodeField[Int](courseJson, "teacher_id")
  
            // 构造 CourseInfo 对象
            CourseInfo(
              courseID = courseIDValue,
              courseCapacity = courseCapacityValue,
              time = timeParsed,
              location = locationValue,
              courseGroupID = courseGroupIDValue,
              teacherID = teacherIDValue,
              preselectedStudentsSize = 0, // 数据表未提供该值，设置为默认值 0
              selectedStudentsSize = 0,   // 数据表未提供该值，设置为默认值 0
              waitingListSize = 0         // 数据表未提供该值，设置为默认值 0
            )
          }
        }
      } yield courseInfo
    }
  }
}
