package model

import java.net.URL
import java.util.UUID

import service.AuthorizationServer
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model.Uri
import akka.http.scaladsl.server.directives.Credentials
import persistence.ClientDAO
import spray.json._



trait Client {

  /**
    * The client SHALL specify if it is:
    * 1)Confidential:
    *   - Clients capable of maintaining the confidentiality of their
    * credentials.
    *
    * 2)Public
    *   - Clients incapable of maintaining the confidentiality of their
    * credential
    *
    * REQUIRED: Don't make assumptions
    *
    * <link>RFC6749#section2.2<link>
    */
  type TypeDesignation
  val typeDesignation: TypeDesignation

  /**
    *
    */
  type ClientID <: String
  val clientID: ClientID


  /**
    *
    */
  type ClientSecret <: String
  val clientSecret: ClientSecret


  type RedirectionURL
  val redirectionURL: RedirectionURL


  val registration: Option[Registration]


  trait Registration {

    /**
      * The authorization server issues the registered client a client
      * identifier -- a unique string representing the registration
      * information provided by the client.
      */
    type Identifier <: String


    /**
      * The identification
      *
      * @return
      */
    def identifier: Identifier


    /**
      * The authorization server SHOULD document the size
      * of any identifier it issues.
      *
      * @return Size of the issued identifier
      */
    def identifierSize: Long = this.identifier.length

    /**
      * Validates issued identifier.
      *
      * @return Accordingly typical implementations, in the identifier is
      *         embedded some sort of temporal component, using, once more,
      *         typical cryptographic methods.
      */
    def isValid: Boolean

  }

}

object ClientType extends Enumeration {
  type Identification = Value
  val Public, Confidential = Value
}

class ClientImpl(private[this] val clientIdentification: ClientType.Value,
                 private[this] val _clientID: String,
                 private[this] val _clientSecret: String,
                 private[this] val _redirectionUrl: List[Uri]
                ) extends Client {

  override type ClientID = String
  override type ClientSecret = String
  override type RedirectionURL = List[Uri]

  override val typeDesignation: ClientType.Value = clientIdentification
  override val clientID: String = clientID
  override val clientSecret: String = clientSecret
  override val redirectionURL: List[Uri] = _redirectionUrl
  override val registration: Option[Registration] = Option(RegistrationImpl(UUID.randomUUID().toString))


  case class RegistrationImpl(identification: String) extends Registration {
    override type Identifier = String

    /**
      * The identification
      *
      * @return
      */
    override def identifier: String = this.identification

    /**
      * Validates issued identifier.
      *
      * @return Accordingly typical implementations, in the identifier is
      *         embedded some sort of temporal component, using, once more,
      *         typical cryptographic methods.
      */
    override def isValid: Boolean = this.identification.isEmpty
  }

  /**
    * The client SHALL specify if it is:
    * 1)Confidential:
    *   - Clients capable of maintaining the confidentiality of their
    * credentials.
    *
    * 2)Public
    *   - Clients incapable of maintaining the confidentiality of their
    * credential
    *
    * REQUIRED: Don't make assumptions
    *
    * <link>RFC6749#section2.2<link>
    */
  override type TypeDesignation = ClientType.Value
}


object ClientImpl {


  /**
    *
    * @param clientRegistration
    * @throws java.net.MalformedURLException
    * @throws IllegalArgumentException
    * @return
    */
  def apply(clientRegistration: ClientRegistrationRequest): Client = {
    clientRegistration.identification match {
      case "public" => new ClientImpl(ClientType.Confidential, UUID.randomUUID().toString, UUID.randomUUID().toString, clientRegistration.redirectionURL.flatMap(url => List(Uri(url))) )
      case "confidential" => new ClientImpl(ClientType.Public, UUID.randomUUID().toString, UUID.randomUUID().toString, clientRegistration.redirectionURL.flatMap(url => List(Uri(url))) )
      case _ => throw new IllegalArgumentException("Client must specify his type")
    }
  }
}


object ClientJsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val clientRegistrationRequest = jsonFormat2(ClientRegistrationRequest)
  implicit val clientRegistrationResponse = jsonFormat2(ClientRegistrationResponse)
}



case class ClientRegistrationRequest(identification: String, redirectionURL: List[String])
case class ClientRegistrationResponse(clientID:String,clientSecret:String)