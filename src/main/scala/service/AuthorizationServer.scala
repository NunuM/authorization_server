package service

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model.{HttpEntity, HttpResponse, StatusCodes}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.directives.Credentials
import akka.http.scaladsl.server.{Directives, PathMatchers, Route}
import akka.stream.ActorMaterializer
import akka.util.Timeout
import model.Messages.{CreateUser, FindUser, LoginUser}
import model._
import org.h2.jdbc.JdbcSQLException
import persistence.UserPersistenceActor
import spray.json.DefaultJsonProtocol
import akka.http.scaladsl.marshallers.xml.ScalaXmlSupport._
import akka.http.scaladsl.unmarshalling.FromRequestUnmarshaller
import org.mindrot.jbcrypt.BCrypt

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.io.StdIn
import scala.util.{Failure, Success}

trait JsonSupport extends UserJsonSupport

trait XmlSupport extends UserXmlSupport

class AuthorizationServer(host: String, port: Int)(implicit val system: ActorSystem)
  extends Directives
    with JsonSupport {

  implicit val materializer = ActorMaterializer()
  implicit val executionContext = system.dispatcher
  implicit val timeout: Timeout = 10.seconds

  private val userPersistenceActor: ActorRef = system.actorOf(Props[UserPersistenceActor], "userpersistence")

  object UserUtil {

    def userBasicAuthenticator(credentials: Credentials): Future[Option[User]] =
      credentials match {
        case password@Credentials.Provided(identifier) =>
          userPersistenceActor
            .ask(LoginUser(identifier))
            .mapTo[Future[Option[(User,String,String)]]]
            .map(_.map((ou) => ou.filter((u) => password.verify(u._2,pass=>{BCrypt.hashpw(pass,u._3)})).map(_._1)))
            .flatten
        case _ => Future.successful(None)
      }
  }

  private val clientRegistrationRoute: Route =

    pathPrefix("user" / PathMatchers.Segment) { (resource) =>
      put {
        decodeRequest {
          entity(as[UserRegistrationRequest]) { clientRequest =>
            encodeResponse {
              complete {
                userPersistenceActor
                  .ask(CreateUser(User(None, resource, clientRequest.username, clientRequest.firstName, clientRequest.email, true), clientRequest.password))
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
      } ~
        Route.seal {
          get {
            authenticateBasicAsync("", UserUtil.userBasicAuthenticator) { user =>
              complete(UserRegistrationResponse(user.username, user.firstName, user.email))
            }
          }
        }
    }

  val route = clientRegistrationRoute

  val bindingFuture = Http().bindAndHandle(route, host, port)

  println(s"Server online at http://localhost:$port/\nPress RETURN to stop...")
  StdIn.readLine()
  bindingFuture
    .flatMap(_.unbind())
    .onComplete(_ => system.terminate())
}
