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
import com.typesafe.sslconfig.akka.AkkaSSLConfig
import facade.OauthHttpFacade
import org.mindrot.jbcrypt.BCrypt

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.io.StdIn
import scala.util.{Failure, Success}
import service.oauth.OauthApplicationService
import service.request.RequestActor
import util.AppConfig
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport

trait JsonSupport extends UserJsonSupport

trait XmlSupport extends UserXmlSupport

class AuthorizationServer(host: String, port: Int)
                         (implicit val system: ActorSystem, implicit val config: AppConfig)
  extends Directives
    with JsonSupport {

  implicit val materializer = ActorMaterializer()
  implicit val executionContext = system.dispatcher
  val sslConfig = AkkaSSLConfig()
  implicit val timeout: Timeout = 10.seconds


  private val userPersistenceActor: ActorRef = system.actorOf(Props[UserPersistenceActor], "userpersistence")
  private val tokenPersistenceActor: ActorRef = system.actorOf(Props[TokenActor], "tokenactor")


  private val oauthHttpFacade = new OauthHttpFacade()

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

    def oauthAuthenticator(credentials: Credentials): Future[Option[User]] =
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
                  .mapTo[Future[Option[User]]]
                  .flatten
                  .map[Future[HttpResponse]]{
                    case Some(res) => {
                      Future.successful(HttpResponse(StatusCodes.InsufficientStorage))
                    }
                    case _ => Future.successful(HttpResponse(StatusCodes.InsufficientStorage))
                  }
              }
            }
          }
        }
      }
    } ~ path("login") {
      get {
        parameter('issuer) { issuer =>
          oauthHttpFacade.consentRedirectResource(issuer) match {
            case Left(uri) => {
              redirect(uri, StatusCodes.TemporaryRedirect)
            }
            case Right(httpResponse) => complete {
              httpResponse
            }
          }
        }
      }
    } ~ path("login" / "facebook" / "authorization") {
      parameter('code.?, 'error_reason.?, 'error.?, 'error_description.?) { (code, errorReason, error, errorDescription) =>

        complete {
          code match {
            case Some(rCode) if rCode.length > 0 => oauthHttpFacade.loginUser(rCode, Issuers.Facebook)
            case _ => HttpResponse(StatusCodes.Unauthorized)
          }
        }
      }
    } ~ path("login" / "google" / "authorization") {
      parameter('code.?, 'error_reason.?, 'error.?, 'error_description.?) { (code, errorReason, error, errorDescription) =>

        complete {
          code match {
            case Some(rCode) => oauthHttpFacade.loginUser(rCode, Issuers.Google)
            case _ => HttpResponse(StatusCodes.Unauthorized)
          }
        }
      }
    } ~ path("authorized") {
      post {
        authenticateOAuth2Async("", UserUtil.oauthAuthenticator) { user =>
          complete("")
        }
      }
    }


  val route = clientRegistrationRoute

  val bindingFuture = Http().bindAndHandle(route, host, port)

  println(s"Server online at http://localhost:$port/\nPress RETURN to stop...")
  val t = StdIn.readLine()

  if("terminate".equals(t)){
    bindingFuture
      .flatMap(_.unbind())
      .onComplete(_ => system.terminate())
  }

}
