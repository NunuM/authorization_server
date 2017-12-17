package model

import akka.http.scaladsl.model.{HttpMethods, HttpRequest, Uri}
import util.AppConfig


trait IssuerApplication {

  implicit val appConfig : AppConfig

  import appConfig.Issuers._

  val name: String

  val identifier: String = appConfig.oauthAppInfo(name)._1

  val secrete: String = appConfig.oauthAppInfo(name)._2

  val scope: Option[List[String]] = Option(appConfig.oauthAppInfo(name)._7)
  
  val consentResource : Uri = appConfig.oauthAppInfo(name)._3

  val authorizationResource : Uri = appConfig.oauthAppInfo(name)._4

  val userResource : Uri = appConfig.oauthAppInfo(name)._5

  val redirectResource : Uri = appConfig.oauthAppInfo(name)._6

  def authorizationRequest: HttpRequest

  def accessTokenRequest(q: Uri.Query): HttpRequest

  def resourceRequest(q: Uri.Query): HttpRequest
}

class FacebookIssuerApplication(implicit val appConfig: AppConfig)
  extends IssuerApplication {

  override val name: String = "facebook"

  /**
    *
    * @return
    */
  override def authorizationRequest = {
    val query = Uri.Query(Map(
      "client_id"-> this.identifier,
      "redirect_uri"-> this.consentResource.toString(),
      "scope" -> this.scope.map(_.mkString(",")).getOrElse("")
    ))

    HttpRequest(HttpMethods.GET, consentResource.withQuery(query))
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

  /**
    *
    * @param q
    * @return
    */
  override def resourceRequest(q: Uri.Query) = {
    val query = Map(
      "fields"-> "email,about,name,first_name,last_name"
    )

    HttpRequest(HttpMethods.GET,userResource.withQuery(Uri.Query(query.++(q.toMap))))
  }

}

class GoogleIssuerApplication((implicit val appConfig: AppConfig)
  extends IssuerApplication {

  override val name: String = "google"

  override def authorizationRequest() = {
    val query = Uri.Query(Map(
      "client_id"-> this.identifier,
      "redirect_uri"-> redirectResource.toString(),
      "scope" -> this.scope.map(_.mkString(",")).getOrElse(""),
      "response_type" -> "code"
    ))

    HttpRequest(HttpMethods.GET, consentResource.withQuery(query))
  }

  override def accessTokenRequest(q: Uri.Query) = {
    val query = Map(
      "client_id"-> this.identifier,
      "redirect_uri"-> redirectResource.toString(),
      "client_secret" -> this.secrete,
      "grant_type" -> "authorization_code"
    )

    HttpRequest(HttpMethods.GET,userResource.withQuery(Uri.Query(query.++(q.toMap))))
  }

  def resourceRequest(q: Uri.Query): HttpRequest = ???
}

