package model

import java.util.UUID

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json._

object Messages {


  case class FindUser(username:String)
  case class CreateUser(user: User,password:String)
  case class LoginUser(username:String)
  case class PreAuth(authToken:String,expiresIn:String,tokenType:String)
  case class IssuerCredential(clientId:String,clientSecrete:String)
  case class OauthLoginUser(user: User,preAuth: PreAuth,issuerCredential: Issuer)
  case class GenerateToken(user: User)
  case class Authenticate(token:UUID)
  case class BearerToken(token_type:String,token:String,expires_in:String)
  case object UpdateToken

  object MessageJsonSupport extends SprayJsonSupport with DefaultJsonProtocol{
    implicit object PreAuthJsonFormat extends RootJsonFormat[PreAuth] {
      def write(preAuth: PreAuth) =
        JsArray(JsString(preAuth.authToken), JsString(preAuth.expiresIn), JsNumber(preAuth.tokenType))

      def read(value: JsValue) = value.asJsObject.getFields("access_token","token_type","expires_in") match {
        case Seq(JsString(access_token), JsString(token_type), JsNumber(expires_in)) =>
          PreAuth(access_token, expires_in.toString(),token_type)
        case _ => deserializationError("PreAuth expected");
      }
    }


    implicit val bearerToken = jsonFormat3(BearerToken)
    implicit val facebookUser = jsonFormat3(FacebookUser)
  }

  case class FacebookUser(name:String,first_name:String,email:String)
  case class Provider(name: String)

  object Implicits{
    implicit def toUser(facebookUser: FacebookUser) : User = {
      User(None,facebookUser.email,facebookUser.first_name,facebookUser.email,true)
    }

  }

}