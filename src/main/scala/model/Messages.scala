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
  case class OauthLoginUser(user: User,preAuth: PreAuth,issuerCredential: IssuerCredential)
  case class GenerateToken(user: User)
  case class Authenticate(token:UUID)
  case class BearerToken(token_type:String,token:String,expires_in:String)
  case object UpdateToken

  object MessageJsonSupport extends SprayJsonSupport with DefaultJsonProtocol{
    implicit object ColorJsonFormat extends RootJsonFormat[PreAuth] {
      def write(preAuth: PreAuth) =
        JsArray(JsString(preAuth.authToken), JsString(preAuth.expiresIn), JsNumber(preAuth.tokenType))

      def read(value: JsValue) = value.asJsObject.getFields("access_token","token_type","expires_in") match {
        case Seq(JsString(access_token), JsString(token_type), JsNumber(expires_in)) =>
          PreAuth(access_token, token_type, expires_in.toString())
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
      User(None,facebookUser.email,facebookUser.email,facebookUser.first_name,facebookUser.email,true)
    }

    implicit def issuer(provider: Provider) : IssuerCredential = {
      provider.name match {
        case "facebook" => IssuerCredential("167287979969308","d9f6c5d384487054aaebb7500793725b")
        case "google" => IssuerCredential("","")
      }
    }

  }

}