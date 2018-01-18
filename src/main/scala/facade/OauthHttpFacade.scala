package facade

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.model.{HttpResponse, ResponseEntity, StatusCodes, Uri}
import akka.http.scaladsl.server.StandardRoute
import akka.stream.ActorMaterializer
import service.oauth.OauthApplicationService
import service.request.RequestActor
import util.AppConfig
import akka.http.scaladsl.model._
import spray.json._
import scala.concurrent.Future
import model.Issuers

class OauthHttpFacade(implicit val appConfig: AppConfig,implicit val system:ActorSystem,implicit val materializer:ActorMaterializer) {

  implicit val executionContext = system.dispatcher
  private val requestServiceActor : ActorRef = system.actorOf(Props[RequestActor],"requestactor")
  private val oauthApplicationService : OauthApplicationService = new OauthApplicationService(requestServiceActor,appConfig)


  def consentRedirectResource(issuers: String) : Either[Uri,HttpResponse] = {
    this.oauthApplicationService
      .find(issuers)
      .map(_.authorizationRedirectResource)
      .toLeft(HttpResponse(StatusCodes.BadRequest))
  }


  def loginUser(code:String,issuer:Issuers.Value): Future[HttpResponse] = {
    val errorResponse = Future(HttpResponse(StatusCodes.InternalServerError))
    val accesToken = this.oauthApplicationService.authorizationCodeFlow(code, issuer)


    accesToken.map {
      case Some(token) => {
        oauthApplicationService.obtainUserFromResourceFlow(token, issuer) match {
          case Right(futureOauthToken) => futureOauthToken.map{
            case Some(token) => {
              import model.Messages.JsonSupport._
              HttpResponse(StatusCodes.OK, entity = token.toJson(oauthBearerTokenJsonSupport).toString())
            }
            case None => HttpResponse(StatusCodes.InternalServerError)
          }
          case Left(e) => errorResponse
        }
      }
      case None => errorResponse
    }.flatten
  }
}
