package model

import akka.actor.ActorSystem
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.model._
import akka.stream.scaladsl.{Flow, Sink}
import akka.util.ByteString
import util.AppConfig
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model.headers.{OAuth2BearerToken, RawHeader}
import akka.stream.ActorMaterializer
import model.FacebookOauthApplication.{BearerToken, UserFromResource}
import spray.json._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


abstract class OauthApplication(val issuer:Issuers.Value) {

  implicit val appConfig : AppConfig

  val name: Issuers.Value = issuer

  val identifier: String = appConfig.oauthAppInfo(name)._1

  val secrete: String = appConfig.oauthAppInfo(name)._2

  val scope: Option[List[String]] = Option(appConfig.oauthAppInfo(name)._7)

  val consentResource : Uri = appConfig.oauthAppInfo(name)._3

  val authorizationResource : Uri = appConfig.oauthAppInfo(name)._4

  val userResource : Uri = appConfig.oauthAppInfo(name)._5

  val redirectResource : Uri = appConfig.oauthAppInfo(name)._6

  def authorizationRedirectResource: Uri

  def accessTokenRequest(q: Uri.Query): HttpRequest

  def unmarshalAccessToken(responseEntity: ResponseEntity) : Future[Option[AccessToken]]

  def unmarshalUserFromResource(responseEntity: ResponseEntity) : Future[Option[User]]

  def resourceRequest(accessToken: AccessToken): HttpRequest

}

class FacebookOauthApplication()(implicit val appConfig: AppConfig,implicit val system:ActorSystem,implicit val materializer:ActorMaterializer)
  extends OauthApplication(Issuers.Facebook) {



  /**
    *
    * @return
    */
  override def authorizationRedirectResource = {
    val query = Uri.Query(Map(
      "client_id"-> this.identifier,
      "redirect_uri"-> this.redirectResource.toString(),
      "scope" -> this.scope.map(_.mkString(",")).getOrElse("")
    ))

    consentResource.withQuery(query)
  }

  /**
    *
    * @param q
    * @return
    */
  override def accessTokenRequest(q: Uri.Query) = {
    val query = Map(
      "client_id"-> this.identifier,
      "redirect_uri"-> redirectResource.toString(),
      "client_secret" -> this.secrete
    )

    HttpRequest(HttpMethods.GET,authorizationResource.withQuery(Uri.Query(query.++(q.toMap))))
  }


  override def unmarshalAccessToken(responseEntity: ResponseEntity): Future[Option[AccessToken]] = {
    import FacebookOauthApplication.JsonSupport._

    val bearerToken = responseEntity
        .dataBytes
        .via(Flow[ByteString].map(_.utf8String.parseJson))
        .map(_.convertTo[FacebookOauthApplication.BearerToken](bearerTokenJsonSupport))


    val token = bearerToken.map((bearerToken) => {
      new AccessToken {
        override type TokenType = String
        override type RefreshToken = String
        override type AccessToken = String
        override type ValidUntil = Long
        override val accessToken = bearerToken.access_token
        override val validUntil = None
        override val tokenType = bearerToken.token_type
        override val refreshToken = None
      }
    })

    token.runWith(Sink.headOption).recover{
      case e:DeserializationException=>{
        println("Eeeeeeeee"+e.getMessage)
        None
      }
    }
  }


  def unmarshalUserFromResource(responseEntity: ResponseEntity) : Future[Option[User]] = {
    import FacebookOauthApplication.JsonSupport._

    val userResource = responseEntity
      .dataBytes
      .via(Flow[ByteString].map(_.utf8String.parseJson))
      .map(_.convertTo[UserFromResource](userFromResourceJsonSupport))

    val user = userResource.map((user) => {

      User(None,user.email,user.first_name,user.email,true)
    })

    user.runWith(Sink.headOption).recover{
      case e:DeserializationException=>{
        println("Eeeeeeeee"+e.getMessage)
        None
      }
    }
  }

  /**
    *
    * @param accessToken: AccessToken
    * @return
    */
  override def resourceRequest(accessToken: AccessToken) = {
    val query = Map(
      "access_token" -> accessToken.accessToken.toString,
      "fields"-> "email,about,name,first_name,last_name"
    )

    HttpRequest(HttpMethods.GET,userResource.withQuery(Uri.Query(query)))
  }

}

object FacebookOauthApplication{

  case class BearerToken(token_type:String,access_token:String)
  case class UserFromResource(id:String,name:String,first_name:String,last_name:String,email:String)

  object JsonSupport extends SprayJsonSupport with DefaultJsonProtocol{
    implicit val bearerTokenJsonSupport = jsonFormat2(BearerToken)
    implicit val userFromResourceJsonSupport = jsonFormat5(UserFromResource)
  }

}


class GoogleOauthApplication(implicit val appConfig: AppConfig,implicit val system:ActorSystem,implicit val materializer:ActorMaterializer)
  extends OauthApplication(Issuers.Google) {


  override def authorizationRedirectResource() = {
    val query = Uri.Query(Map(
      "client_id"-> this.identifier,
      "redirect_uri"-> redirectResource.toString(),
      "scope" -> this.scope.map(_.mkString(",")).getOrElse(""),
      "response_type" -> "code"
    ))

    consentResource.withQuery(query)
  }

  override def accessTokenRequest(q: Uri.Query) = {
    val query = Map(
      "client_id"-> this.identifier,
      "redirect_uri"-> redirectResource.toString(),
      "client_secret" -> this.secrete,
      "grant_type" -> "authorization_code",
      "scope" -> ""
    ) ++ q.toMap

    val contentType = RawHeader("content-type", "application/x-www-form-urlencoded")

    HttpRequest(HttpMethods.PUT,this.authorizationResource,List(contentType),FormData(query).toEntity)
  }


  def resourceRequest(accessToken: AccessToken): HttpRequest = ???

  def unmarshalUserFromResource(responseEntity: ResponseEntity) : Future[Option[User]] = ???

  override def unmarshalAccessToken(responseEntity: ResponseEntity): Future[Option[AccessToken]] = {
  import GoogleOauthApplication.JsonSupport._
    val bearerToken = responseEntity
      .dataBytes
      .via(Flow[ByteString].map(tk =>{
        println(tk.utf8String)
        tk.utf8String.parseJson
      }))
      .map(_.convertTo[GoogleOauthApplication.BearerToken](bearerTokenJsonSupport))


    val token = bearerToken.map((bearerToken) => {
      new AccessToken {
        override type TokenType = String
        override type RefreshToken = String
        override type AccessToken = String
        override type ValidUntil = Long
        override val accessToken = bearerToken.access_token
        override val validUntil = None
        override val tokenType = bearerToken.token_type
        override val refreshToken = None
      }
    })

    token.runWith(Sink.headOption).recover{
      case e:DeserializationException=>{
        println("Eeeeeeeee"+e.getMessage)
        None
      }
    }
  }

}

object GoogleOauthApplication{
  case class BearerToken(token_type:String,access_token:String)
  case class UserFromResource(id:String,name:String,first_name:String,last_name:String,email:String)

  object JsonSupport extends SprayJsonSupport with DefaultJsonProtocol{
    implicit val bearerTokenJsonSupport = jsonFormat2(BearerToken)
    implicit val userFromResourceJsonSupport = jsonFormat5(UserFromResource)
  }
}

