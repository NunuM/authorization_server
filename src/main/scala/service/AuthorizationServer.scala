package service

import akka.NotUsed
import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.model.HttpMethods
import akka.pattern.ask
import akka.http.scaladsl.Http
import akka.stream.scaladsl._
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.directives.Credentials
import akka.http.scaladsl.server.{Directives, PathMatchers, Route}
import akka.stream._
import akka.util.{ByteString, Timeout}
import model.Messages._
import model._
import org.h2.jdbc.JdbcSQLException
import persistence.{TokenActor, UserPersistenceActor}
import spray.json.{DefaultJsonProtocol, DeserializationException, JsValue}
import akka.http.scaladsl.marshallers.xml.ScalaXmlSupport._
import akka.http.scaladsl.unmarshalling.FromRequestUnmarshaller
import akka.stream.impl.fusing.GraphStages.FutureSource
import akka.stream.scaladsl.{Flow, GraphDSL, RunnableGraph, Source}
import org.mindrot.jbcrypt.BCrypt

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.io.StdIn
import scala.util.{Failure, Success}
import Messages.Implicits._
import util.AppConfig

trait JsonSupport extends UserJsonSupport

trait XmlSupport extends UserXmlSupport

class AuthorizationServer(host: String, port: Int)
                         (implicit val system: ActorSystem, implicit val config: AppConfig)
  extends Directives
    with JsonSupport {

  implicit val materializer = ActorMaterializer()
  implicit val executionContext = system.dispatcher
  implicit val timeout: Timeout = 10.seconds

  import Messages.MessageJsonSupport._


  private val userPersistenceActor: ActorRef = system.actorOf(Props[UserPersistenceActor], "userpersistence")
  private val tokenPersistenceActor: ActorRef = system.actorOf(Props[TokenActor], "tokenactor")

  object UserUtil {

    def userBasicAuthenticator(credentials: Credentials): Future[Option[User]] =
      credentials match {
        case password@Credentials.Provided(identifier) =>
          userPersistenceActor
            .ask(LoginUser(identifier))
            .mapTo[Future[Option[(User, String, String)]]]
            .map(_.map((ou) => ou.filter((u) => password.verify(u._2, pass => {
              BCrypt.hashpw(pass, u._3)
            })).map(_._1)))
            .flatten
        case _ => Future.successful(None)
      }
  }

  private val clientRegistrationRoute: Route =

    pathPrefix("user") {
      post {
        decodeRequest {
          entity(as[UserRegistrationRequest]) { clientRequest =>
            encodeResponse {
              complete {
                userPersistenceActor
                  .ask(CreateUser(User(None, clientRequest.username, clientRequest.firstName, clientRequest.email, true), clientRequest.password))
                  .mapTo[Future[Option[Unit]]]
                  .transformWith[HttpResponse] {
                  case Success(result) => result.map {
                    case Some(res) => HttpResponse(StatusCodes.Created)
                    case _ => HttpResponse(StatusCodes.InsufficientStorage)
                  }.recover {
                    case jdbc: JdbcSQLException => HttpResponse(StatusCodes.Forbidden)
                    case _ => HttpResponse(StatusCodes.InternalServerError)
                  }
                  case Failure(e) => Future(HttpResponse(StatusCodes.InternalServerError))
                }
              }
            }
          }
        }
      }
    } ~ path("login") {
      get {
        parameter('issuer) { issuer =>
          config.issuer(issuer) match {
            case Some(i) => {
              println(i.authorizationDialog)
              redirect(i.authorizationDialog, StatusCodes.TemporaryRedirect)
            }
            case _ => complete {
              HttpResponse(StatusCodes.BadRequest)
            }
          }
        }
      }
    } ~ path("login" / "facebook" / "authorization") {
      parameter('code.?, 'error_reason.?, 'error.?, 'error_description.?) { (code, errorReason, error, errorDescription) =>
        val issuer = config.facebookIssuer

        complete {
          code match {
            case Some(rCode) => oauthGrantFlow(issuer.accessTokenURL(Uri.Query(Map("code" -> rCode))), issuer)
            case _ => HttpResponse(StatusCodes.Unauthorized)
          }
        }
      }
    } ~ path("login" / "google" / "authorization") {
      parameter('code.?, 'error_reason.?, 'error.?, 'error_description.?) { (code, errorReason, error, errorDescription) =>
        val issuer = config.googleIssuer

        complete {
          code match {
            case Some(rCode) => googleOauthGrantFlow(issuer.accessTokenURL(Uri.Query(Map("code" -> rCode))), issuer)
            case _ => HttpResponse(StatusCodes.Unauthorized)
          }
        }
      }
    } ~ path("authorized") {
      post {
        complete {
          ""
        }
      }
    }


  def oauthGrantFlow(tokenUrl: Uri, provider: Issuer): Future[BearerToken] = {

    println(tokenUrl)
    val response = for {
      tokenResponse <- Http().singleRequest(HttpRequest(HttpMethods.GET, tokenUrl)).map(_.entity.dataBytes)
      preAuth <- tokenResponse.via(unmarshallerEntity.map(_.convertTo[PreAuth])).runWith(Sink.head)
      responseFromResource <- Http().singleRequest(HttpRequest(HttpMethods.GET, provider.resourceOwnerInfo(Uri.Query(Map("access_token"-> preAuth.authToken))))).map(_.entity.dataBytes)
      userFromResource <- responseFromResource.via(unmarshallerEntity.map(_.convertTo[FacebookUser])).runWith(Sink.head)
      user <- userPersistenceActor.ask(OauthLoginUser(toUser(userFromResource), preAuth, provider)).mapTo[Future[User]]
    } yield {
      user.map(u => tokenPersistenceActor.ask(GenerateToken(u)).mapTo[BearerToken]).flatten
    }

    response.recover {
      case e: DeserializationException => HttpResponse(StatusCodes.InsufficientStorage)
      case e: JdbcSQLException => HttpResponse(StatusCodes.InsufficientStorage)
    }

    response.flatten
  }


  def googleOauthGrantFlow(tokenUrl: Uri, provider: Issuer): Future[BearerToken] = {

    println(tokenUrl)
    val response = for {
      tokenResponse <- Http().singleRequest(HttpRequest(HttpMethods.GET, tokenUrl)).map(_.entity.dataBytes)
      preAuth <- tokenResponse.via(unmarshallerEntity.map(_.convertTo[PreAuth])).runWith(Sink.head)
      responseFromResource <- Http().singleRequest(HttpRequest(HttpMethods.GET, provider.resourceOwnerInfo(Uri.Query(Map("access_token"-> preAuth.authToken))))).map(_.entity.dataBytes)
      userFromResource <- responseFromResource.via(unmarshallerEntity.map(_.convertTo[FacebookUser])).runWith(Sink.head)
      user <- userPersistenceActor.ask(OauthLoginUser(toUser(userFromResource), preAuth, provider)).mapTo[Future[User]]
    } yield {
      user.map(u => tokenPersistenceActor.ask(GenerateToken(u)).mapTo[BearerToken]).flatten
    }

    response.recover {
      case e: DeserializationException => HttpResponse(StatusCodes.InsufficientStorage)
      case e: JdbcSQLException => HttpResponse(StatusCodes.InsufficientStorage)
    }

    response.flatten
  }



  private def unmarshallerEntity: Flow[ByteString, JsValue, NotUsed] = {
    Flow[ByteString].map(_.utf8String).map((value) => {
      import spray.json._
      value.parseJson
    })
  }

  private def makeRequest(uri: Uri): Option[HttpResponse] = {
    val result = Http().singleRequest(HttpRequest(HttpMethods.GET, uri)).filter(_.status.intValue() == StatusCodes.OK.intValue).map(Option(_))
    Await.result(result, Duration.Inf)
  }

  val route = clientRegistrationRoute

  val bindingFuture = Http().bindAndHandle(route, host, port)

  println(s"Server online at http://localhost:$port/\nPress RETURN to stop...")
  StdIn.readLine()
  bindingFuture
    .flatMap(_.unbind())
    .onComplete(_ => system.terminate())
}
