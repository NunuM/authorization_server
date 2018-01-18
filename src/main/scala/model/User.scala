package model

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.marshallers.xml.ScalaXmlSupport
import akka.http.scaladsl.model._
import akka.http.scaladsl.unmarshalling.{FromEntityUnmarshaller, Unmarshaller}
import spray.json._

import scala.xml.NodeSeq


case class User(id: Option[Long], username: String, firstName: String, email: String, isActive: Boolean)

case class UserRegistrationRequest(username: String, firstName: String, email: String, password: String)

case class UserRegistrationResponse(username: String, firstName: String, email: String)

trait UserJsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val userRegistrationRequestJSON = jsonFormat4(UserRegistrationRequest)
  implicit val userRegistrationResponseJSON = jsonFormat3(UserRegistrationResponse)
}

trait UserXmlSupport extends ScalaXmlSupport {


}