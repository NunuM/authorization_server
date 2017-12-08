package model

import akka.http.scaladsl.server.directives.Credentials
import persistence.{UserDao}
import spray.json.DefaultJsonProtocol
import spray.json._
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport


case class User(username:String,password:String)

object UserJsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val userRegistrationRequest = jsonFormat2(User)
}


object UserUtil{
  def authenticate(credentials: Credentials): Option[User] = {
    credentials match {
      case password @ Credentials.Provided(user) => {
        UserDao.find(user) match {
          case Some(client) if password.verify(client.password) => Some(client)
          case _ => None
        }
      }
      case _ => None
    }
  }

}
