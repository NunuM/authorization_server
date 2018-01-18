package model

import java.util.UUID

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model.HttpRequest
import spray.json._

object Messages {

  case class MakeRequest(httpRequest: HttpRequest)

  case class FindUser(username:String)
  case class CreateUser(user: User,password:String)
  case class LoginUser(username:String)
  case class PreAuth(authToken:String,expiresIn:String,tokenType:String)
  case class IssuerCredential(clientId:String,clientSecrete:String)
  case class OauthLoginUser(user: User,accessToken: AccessToken,issuerCredential: OauthApplication)
  case class GenerateToken(user: User)
  case class Authenticate(token:UUID)

  case object UpdateToken


  object JsonSupport extends SprayJsonSupport with DefaultJsonProtocol{
    val oauthBearerTokenJsonSupport = jsonFormat4(OauthBearerToken)
  }

}

