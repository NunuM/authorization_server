package service.oauth

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.model.headers.OAuth2BearerToken
import akka.http.scaladsl.model.{HttpResponse, Uri}
import akka.pattern.ask
import akka.stream.ActorMaterializer
import akka.util.Timeout
import model.Messages.{GenerateToken, MakeRequest, OauthLoginUser}
import model._
import persistence.{TokenActor, UserPersistenceActor}
import util.AppConfig

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._

class OauthApplicationService(private val requestActor: ActorRef, implicit val appConfig: AppConfig)
                             (implicit val system: ActorSystem, implicit val materializer: ActorMaterializer) {

  implicit val timeout: Timeout = 10.seconds

  implicit val executionContext = system.dispatcher

  private val applications: collection.mutable.Map[Issuers.Value, OauthApplication] = new mutable.HashMap[Issuers.Value, OauthApplication]()
  private val userPersistenceActor: ActorRef = system.actorOf(Props[UserPersistenceActor])
  private val tokenPersistenceActor: ActorRef = system.actorOf(Props[TokenActor])

  applications += (Issuers.Facebook -> new FacebookOauthApplication())
  applications += (Issuers.Google -> new GoogleOauthApplication())

  def find(issuer: String): Option[OauthApplication] = {
    Issuers.fromString(issuer) match {
      case Some(i) => this.applications.get(i)
      case None => None
    }
  }


  def authorizationCodeFlow(code: String, issuer: Issuers.Value): Future[Option[AccessToken]] = {
    val eitherOauthApplication = this.applications.get(issuer)

    val tokenRequest = eitherOauthApplication.map(_.accessTokenRequest(Uri.Query("code" -> code)))

    val optionFutureTokenResponse = tokenRequest.map(request => this.requestActor.ask(MakeRequest(request)).mapTo[Future[HttpResponse]])

    val oftr = optionFutureTokenResponse.map(_.flatten)

    val token = oftr.toLeft(None)

    token.left.map(_.map(_.entity).map(eitherOauthApplication.get.unmarshalAccessToken).flatten) match {
      case Left(value) => value
      case Right(_) => Future(None)
    }
  }


  def obtainUserFromResourceFlow(accessToken: AccessToken, issuer: Issuers.Value): Either[Throwable, Future[Option[OauthBearerToken]]] = {
    val eitherOauthApplication = this.applications.get(issuer).toRight(throw new IllegalArgumentException("Issuer not exists"))

    val result = eitherOauthApplication.right.map[Future[Option[OauthBearerToken]]](oauthApplication => {
      val userRequest = oauthApplication.resourceRequest(accessToken)

      val response = this.requestActor.ask(MakeRequest(userRequest)).mapTo[Future[HttpResponse]].flatten

      val userToSave = response.map(_.entity).map(oauthApplication.unmarshalUserFromResource).flatten

      val l = for {
        user <- userToSave.map(_.map(ui => userPersistenceActor.ask(OauthLoginUser(ui, accessToken, oauthApplication)).mapTo[Future[User]].flatten))
      } yield {
        user match {
          case Some(u) => Option(u.map(ui => tokenPersistenceActor.ask(GenerateToken(ui)).mapTo[OauthBearerToken]))
          case None => None
        }
      }

      val k = l.map(_.map(_.flatten))

      k.flatMap(optionToFuture[OauthBearerToken])
    })

    result
  }


  private def optionToFuture[A](x: Option[Future[A]])(implicit ec: ExecutionContext): Future[Option[A]] =
    x match {
      case Some(f) => f.map(Some(_))
      case None    => Future.successful(None)
    }

}
