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
import spray.json.DefaultJsonProtocol
import akka.http.scaladsl.marshallers.xml.ScalaXmlSupport._
import akka.http.scaladsl.unmarshalling.FromRequestUnmarshaller
import akka.stream.impl.fusing.GraphStages.FutureSource
import akka.stream.scaladsl.{Flow, GraphDSL, RunnableGraph, Source}
import org.mindrot.jbcrypt.BCrypt

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.io.StdIn
import scala.util.{Failure, Success}
import Messages.Implicits._

trait JsonSupport extends UserJsonSupport

trait XmlSupport extends UserXmlSupport

class AuthorizationServer(host: String, port: Int)(implicit val system: ActorSystem)
  extends Directives
    with JsonSupport {

  implicit val materializer = ActorMaterializer()
  implicit val executionContext = system.dispatcher
  implicit val timeout: Timeout = 10.seconds
  import Messages.MessageJsonSupport._


  private val userPersistenceActor: ActorRef = system.actorOf(Props[UserPersistenceActor], "userpersistence")
  private val tokenPersistenceActor:ActorRef = system.actorOf(Props[TokenActor],"tokenactor")

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
    } ~ path("login") {
      get {
        parameter('issuer) {
          case "facebook" => redirect(s"https://www.facebook.com/v2.11/dialog/oauth?client_id=167287979969308&redirect_uri=http://localhost:9090/login/facebook/authorization&scope=email,public_profile,user_about_me", StatusCodes.TemporaryRedirect)
          case "google" => redirect("", StatusCodes.TemporaryRedirect)
          case _ => complete {
            HttpResponse(StatusCodes.BadRequest)
          }
        }
      }
    } ~ path("login" / "facebook" / "authorization") {
      parameter('code) { code =>
        println(l + ":::" + code)

        val url = s"https://graph.facebook.com/v2.11/oauth/access_token?client_id=167287979969308&redirect_uri=http://localhost:9090/login/facebook/authorization&client_secret=d9f6c5d384487054aaebb7500793725b&code=$code"
        complete {
          oauthGrantFlow(url,Provider("facebook"))
        }
      }
    }

  val l = 1

  def oauthGrantFlow(url:String,provider:Provider) : Future[BearerToken] = {

    val response = for{
      tokenResponse <- Http().singleRequest(HttpRequest(HttpMethods.GET,Uri(url))).map(_.entity.dataBytes)
      preAuth <- tokenResponse.via(mapEntityToTuple).runWith(Sink.head)
      responseFromResource <- Http().singleRequest(HttpRequest(HttpMethods.GET, Uri(s"https://graph.facebook.com/v2.11/me?fields=email,about,name,first_name,last_name&access_token=${preAuth.authToken}"))).map(_.entity.dataBytes)
      userFromResource <- responseFromResource.via(mapEntityToTuple2).runWith(Sink.head)
      user <- userPersistenceActor.ask(OauthLoginUser(toUser(userFromResource),preAuth,issuer(provider))).mapTo[Future[User]]
    } yield {
      user.map(u => tokenPersistenceActor.ask(GenerateToken(u)).mapTo[BearerToken]).flatten
    }

    response.flatten
  }

  val mapEntityToTuple = Flow[ByteString].map(_.utf8String).map((value) => {
    import spray.json._
    import Messages.MessageJsonSupport._
    value.parseJson.convertTo[PreAuth]
  })

  val mapEntityToTuple2 = Flow[ByteString].map(_.utf8String).map((value) => {
    import spray.json._
    import Messages.MessageJsonSupport._
    value.parseJson.convertTo[FacebookUser]
  })

  val route = clientRegistrationRoute

  val bindingFuture = Http().bindAndHandle(route, host, port)

  println(s"Server online at http://localhost:$port/\nPress RETURN to stop...")
  StdIn.readLine()
  bindingFuture
    .flatMap(_.unbind())
    .onComplete(_ => system.terminate())
}
