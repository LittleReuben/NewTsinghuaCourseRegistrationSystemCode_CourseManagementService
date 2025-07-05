
package Process

import Common.API.{API, PlanContext, TraceID}
import Common.DBAPI.{initSchema, writeDB}
import Common.ServiceUtils.schemaName
import Global.ServerConfig
import cats.effect.IO
import io.circe.generic.auto.*
import java.util.UUID
import Global.DBConfig
import Process.ProcessUtils.server2DB
import Global.GlobalVariables

object Init {
  def init(config: ServerConfig): IO[Unit] = {
    given PlanContext = PlanContext(traceID = TraceID(UUID.randomUUID().toString), 0)
    given DBConfig = server2DB(config)

    val program: IO[Unit] = for {
      _ <- IO(GlobalVariables.isTest=config.isTest)
      _ <- API.init(config.maximumClientConnection)
      _ <- Common.DBAPI.SwitchDataSourceMessage(projectName = Global.ServiceCenter.projectName).send
      _ <- initSchema(schemaName)
            /** 授权教师表，关联到课程组
       * tmp: 占位主键
       * course_group_id: 所属课程组ID，关联到CourseGroupTable中的course_group_id
       * authorized_teacher_id: 授权教师ID
       */
      _ <- writeDB(
        s"""
        CREATE TABLE IF NOT EXISTS "${schemaName}"."authorized_teachers_table" (
            tmp SERIAL NOT NULL PRIMARY KEY,
            course_group_id INT NOT NULL,
            authorized_teacher_id INT NOT NULL
        );
         
        """,
        List()
      )
      /** 课程组表，记录课程组的基本信息
       * course_group_id: 课程组ID，主键，自动递增
       * name: 课程组名字
       * credit: 课程组学分
       * owner_teacher_id: 创建课程组的老师ID
       */
      _ <- writeDB(
        s"""
        CREATE TABLE IF NOT EXISTS "${schemaName}"."course_group_table" (
            course_group_id SERIAL NOT NULL PRIMARY KEY,
            name TEXT NOT NULL,
            credit INT NOT NULL,
            owner_teacher_id INT NOT NULL
        );
         
        """,
        List()
      )
      /** 课程表，包含每门课程的基本信息
       * course_id: 课程的唯一编号，主键，自增
       * course_group_id: 所属课程组ID，外键，关联到CourseGroupTable中的course_group_id
       * course_capacity: 课程容量，表示该课程最多可容纳学生人数
       * time: 上课时间（格式化为JSON，包含day_of_week和time_period）
       * location: 上课地点
       * teacher_id: 开设该门课程的老师ID
       */
      _ <- writeDB(
        s"""
        CREATE TABLE IF NOT EXISTS "${schemaName}"."course_table" (
            course_id SERIAL NOT NULL PRIMARY KEY,
            course_group_id INT NOT NULL,
            course_capacity INT,
            time TEXT NOT NULL,
            location TEXT,
            teacher_id INT NOT NULL
        );
         
        """,
        List()
      )
    } yield ()

    program.handleErrorWith(err => IO {
      println("[Error] Process.Init.init 失败, 请检查 db-manager 是否启动及端口问题")
      err.printStackTrace()
    })
  }
}
    