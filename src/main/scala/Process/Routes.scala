
package Process

import Common.API.PlanContext
import Common.DBAPI.DidRollbackException
import cats.effect.*
import fs2.concurrent.Topic
import io.circe.*
import io.circe.derivation.Configuration
import io.circe.generic.auto.*
import io.circe.parser.decode
import io.circe.syntax.*
import org.http4s.*
import org.http4s.client.Client
import org.http4s.dsl.io.*
import scala.collection.concurrent.TrieMap
import Common.Serialize.CustomColumnTypes.*
import Impl.QueryCourseGroupAuthorizedTeachersMessagePlanner
import Impl.RevokeCourseGroupAuthorizationMessagePlanner
import Impl.CreateCourseGroupMessagePlanner
import Impl.QueryOwnCoursesMessagePlanner
import Impl.QueryOwnCourseGroupsMessagePlanner
import Impl.QueryCoursesByFilterMessagePlanner
import Impl.QueryCourseGroupByIDMessagePlanner
import Impl.DeleteCourseMessagePlanner
import Impl.QueryCourseGroupAuthorizationReceivedMessagePlanner
import Impl.DeleteCourseGroupMessagePlanner
import Impl.UpdateCourseGroupInfoMessagePlanner
import Impl.QueryCourseByIDMessagePlanner
import Impl.QueryCoursesByCourseGroupMessagePlanner
import Impl.CreateCourseMessagePlanner
import Impl.GrantCourseGroupAuthorizationMessagePlanner
import Impl.UpdateCourseMessagePlanner
import Common.API.TraceID
import org.joda.time.DateTime
import org.http4s.circe.*
import java.util.UUID
import Common.Serialize.CustomColumnTypes.{decodeDateTime,encodeDateTime}

object Routes:
  val projects: TrieMap[String, Topic[IO, String]] = TrieMap.empty

  private def executePlan(messageType: String, str: String): IO[String] =
    messageType match {
      case "QueryCourseGroupAuthorizedTeachersMessage" =>
        IO(
          decode[QueryCourseGroupAuthorizedTeachersMessagePlanner](str) match
            case Left(err) => err.printStackTrace(); throw new Exception(s"Invalid JSON for QueryCourseGroupAuthorizedTeachersMessage[${err.getMessage}]")
            case Right(value) => value.fullPlan.map(_.asJson.toString)
        ).flatten
       
      case "RevokeCourseGroupAuthorizationMessage" =>
        IO(
          decode[RevokeCourseGroupAuthorizationMessagePlanner](str) match
            case Left(err) => err.printStackTrace(); throw new Exception(s"Invalid JSON for RevokeCourseGroupAuthorizationMessage[${err.getMessage}]")
            case Right(value) => value.fullPlan.map(_.asJson.toString)
        ).flatten
       
      case "CreateCourseGroupMessage" =>
        IO(
          decode[CreateCourseGroupMessagePlanner](str) match
            case Left(err) => err.printStackTrace(); throw new Exception(s"Invalid JSON for CreateCourseGroupMessage[${err.getMessage}]")
            case Right(value) => value.fullPlan.map(_.asJson.toString)
        ).flatten
       
      case "QueryOwnCoursesMessage" =>
        IO(
          decode[QueryOwnCoursesMessagePlanner](str) match
            case Left(err) => err.printStackTrace(); throw new Exception(s"Invalid JSON for QueryOwnCoursesMessage[${err.getMessage}]")
            case Right(value) => value.fullPlan.map(_.asJson.toString)
        ).flatten
       
      case "QueryOwnCourseGroupsMessage" =>
        IO(
          decode[QueryOwnCourseGroupsMessagePlanner](str) match
            case Left(err) => err.printStackTrace(); throw new Exception(s"Invalid JSON for QueryOwnCourseGroupsMessage[${err.getMessage}]")
            case Right(value) => value.fullPlan.map(_.asJson.toString)
        ).flatten
       
      case "QueryCoursesByFilterMessage" =>
        IO(
          decode[QueryCoursesByFilterMessagePlanner](str) match
            case Left(err) => err.printStackTrace(); throw new Exception(s"Invalid JSON for QueryCoursesByFilterMessage[${err.getMessage}]")
            case Right(value) => value.fullPlan.map(_.asJson.toString)
        ).flatten
       
      case "QueryCourseGroupByIDMessage" =>
        IO(
          decode[QueryCourseGroupByIDMessagePlanner](str) match
            case Left(err) => err.printStackTrace(); throw new Exception(s"Invalid JSON for QueryCourseGroupByIDMessage[${err.getMessage}]")
            case Right(value) => value.fullPlan.map(_.asJson.toString)
        ).flatten
       
      case "DeleteCourseMessage" =>
        IO(
          decode[DeleteCourseMessagePlanner](str) match
            case Left(err) => err.printStackTrace(); throw new Exception(s"Invalid JSON for DeleteCourseMessage[${err.getMessage}]")
            case Right(value) => value.fullPlan.map(_.asJson.toString)
        ).flatten
       
      case "QueryCourseGroupAuthorizationReceivedMessage" =>
        IO(
          decode[QueryCourseGroupAuthorizationReceivedMessagePlanner](str) match
            case Left(err) => err.printStackTrace(); throw new Exception(s"Invalid JSON for QueryCourseGroupAuthorizationReceivedMessage[${err.getMessage}]")
            case Right(value) => value.fullPlan.map(_.asJson.toString)
        ).flatten
       
      case "DeleteCourseGroupMessage" =>
        IO(
          decode[DeleteCourseGroupMessagePlanner](str) match
            case Left(err) => err.printStackTrace(); throw new Exception(s"Invalid JSON for DeleteCourseGroupMessage[${err.getMessage}]")
            case Right(value) => value.fullPlan.map(_.asJson.toString)
        ).flatten
       
      case "UpdateCourseGroupInfoMessage" =>
        IO(
          decode[UpdateCourseGroupInfoMessagePlanner](str) match
            case Left(err) => err.printStackTrace(); throw new Exception(s"Invalid JSON for UpdateCourseGroupInfoMessage[${err.getMessage}]")
            case Right(value) => value.fullPlan.map(_.asJson.toString)
        ).flatten
       
      case "QueryCourseByIDMessage" =>
        IO(
          decode[QueryCourseByIDMessagePlanner](str) match
            case Left(err) => err.printStackTrace(); throw new Exception(s"Invalid JSON for QueryCourseByIDMessage[${err.getMessage}]")
            case Right(value) => value.fullPlan.map(_.asJson.toString)
        ).flatten
       
      case "QueryCoursesByCourseGroupMessage" =>
        IO(
          decode[QueryCoursesByCourseGroupMessagePlanner](str) match
            case Left(err) => err.printStackTrace(); throw new Exception(s"Invalid JSON for QueryCoursesByCourseGroupMessage[${err.getMessage}]")
            case Right(value) => value.fullPlan.map(_.asJson.toString)
        ).flatten
       
      case "CreateCourseMessage" =>
        IO(
          decode[CreateCourseMessagePlanner](str) match
            case Left(err) => err.printStackTrace(); throw new Exception(s"Invalid JSON for CreateCourseMessage[${err.getMessage}]")
            case Right(value) => value.fullPlan.map(_.asJson.toString)
        ).flatten
       
      case "GrantCourseGroupAuthorizationMessage" =>
        IO(
          decode[GrantCourseGroupAuthorizationMessagePlanner](str) match
            case Left(err) => err.printStackTrace(); throw new Exception(s"Invalid JSON for GrantCourseGroupAuthorizationMessage[${err.getMessage}]")
            case Right(value) => value.fullPlan.map(_.asJson.toString)
        ).flatten
       
      case "UpdateCourseMessage" =>
        IO(
          decode[UpdateCourseMessagePlanner](str) match
            case Left(err) => err.printStackTrace(); throw new Exception(s"Invalid JSON for UpdateCourseMessage[${err.getMessage}]")
            case Right(value) => value.fullPlan.map(_.asJson.toString)
        ).flatten
       

      case "test" =>
        for {
          output  <- Utils.Test.test(str)(using  PlanContext(TraceID(""), 0))
        } yield output
      case _ =>
        IO.raiseError(new Exception(s"Unknown type: $messageType"))
    }

  def handlePostRequest(req: Request[IO]): IO[String] = {
    req.as[Json].map { bodyJson =>
      val hasPlanContext = bodyJson.hcursor.downField("planContext").succeeded

      val updatedJson = if (hasPlanContext) {
        bodyJson
      } else {
        val planContext = PlanContext(TraceID(UUID.randomUUID().toString), transactionLevel = 0)
        val planContextJson = planContext.asJson
        bodyJson.deepMerge(Json.obj("planContext" -> planContextJson))
      }
      updatedJson.toString
    }
  }
  val service: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case GET -> Root / "health" =>
      Ok("OK")
      
    case GET -> Root / "stream" / projectName =>
      projects.get(projectName) match {
        case Some(topic) =>
          val stream = topic.subscribe(10)
          Ok(stream)
        case None =>
          Topic[IO, String].flatMap { topic =>
            projects.putIfAbsent(projectName, topic) match {
              case None =>
                val stream = topic.subscribe(10)
                Ok(stream)
              case Some(existingTopic) =>
                val stream = existingTopic.subscribe(10)
                Ok(stream)
            }
          }
      }
    case req@POST -> Root / "api" / name =>
      handlePostRequest(req).flatMap {
        executePlan(name, _)
      }.flatMap(Ok(_))
      .handleErrorWith {
        case e: DidRollbackException =>
          println(s"Rollback error: $e")
          val headers = Headers("X-DidRollback" -> "true")
          BadRequest(e.getMessage.asJson.toString).map(_.withHeaders(headers))

        case e: Throwable =>
          println(s"General error: $e")
          BadRequest(e.getMessage.asJson.toString)
      }
  }
  