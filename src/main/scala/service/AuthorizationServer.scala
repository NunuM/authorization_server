package service

import akka.NotUsed
import akka.actor.Status.Success
import akka.http.scaladsl.server.PathMatchers
import akka.http.scaladsl.common.JsonEntityStreamingSupport
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.{HttpApp, Route}
import akka.stream.Materializer
import akka.stream.scaladsl.Source
import model._
import persistence.{ClientDAO, UserDao}
import service.AuthorizationServer.entity
import akka.http.scaladsl.server.Directives._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Try}

object AuthorizationServer extends HttpApp {


  import model.UserJsonSupport.userRegistrationRequest
  import ClientJsonSupport._
  import scala.concurrent.ExecutionContext.Implicits.global

  /**
    * Authentication Server authenticates the resource owner
    * The client never sees the credentials
    *
    * Exchanging Code
    *
    * Client Authentication -> Authorization Grant
    *
    * refresh Tokens
    *
    * @return
    */
  override protected def routes: Route = {
    clientRegistrationRoute
  }


  private val clientRoutes: Route = {
    clientRegistrationRoute
  }

  private val clientRegistrationRoute: Route =
    pathPrefix("user") {
      put {
        decodeRequest {
          entity(as[User]) { clientRequest =>
            encodeResponse {
              complete {
                Try(clientRequest)
                  .map(UserDao.create)
                  .map((storedClient) => {
                    storedClient.map {
                      case cl: model.Client => HttpResponse(StatusCodes.OK)
                    }
                  })
                  .recover {
                    case ie: IllegalArgumentException => Future(HttpResponse(StatusCodes.BadRequest))
                    case ue: java.net.MalformedURLException => Future(HttpResponse(StatusCodes.BadRequest, entity = ""))
                  }
              }
            }
          }
        }
      } ~
        path(PathMatchers.Segment) { (username) =>
          authenticateBasic("",UserUtil.authenticate) { (user) =>
            get {
              complete(user.username)
            }
          }
        }
    }
}
