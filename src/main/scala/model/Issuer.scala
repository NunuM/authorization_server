package model

import akka.http.scaladsl.model.Uri

trait Issuer {
  val identifier: String
  val secrete: String

  val scope: Option[List[String]]

  def authorizationDialog: Uri

  def accessTokenURL(q: Uri.Query): Uri

  def resourceOwnerInfo(q: Uri.Query): Uri
}

class FacebookIssuer(override val identifier: String,
                     override val secrete: String,
                     override val scope: Option[List[String]],
                     authorizationDialogUrl: Uri,
                     accessTokenUrl: Uri,
                     resourceUrl: Uri,
                     redirectUrl:Uri
                    ) extends Issuer {

  override def authorizationDialog = {
    val query = Uri.Query(Map(
      "client_id"-> this.identifier,
      "redirect_uri"-> redirectUrl.toString(),
      "scope" -> this.scope.map(_.mkString(",")).getOrElse("")
    ))

    authorizationDialogUrl.withQuery(query)
  }

  override def accessTokenURL(q: Uri.Query) = {
    val query = Map(
      "client_id"-> this.identifier,
      "redirect_uri"-> redirectUrl.toString(),
      "client_secret" -> this.secrete
    )

    accessTokenUrl.withQuery(Uri.Query(query.++(q.toMap)))
  }

  override def resourceOwnerInfo(q: Uri.Query) = {
    val query = Map(
      "fields"-> "email,about,name,first_name,last_name"
    )

    resourceUrl.withQuery(Uri.Query(query.++(q.toMap)))
  }
}

class GoogleIssuer(override val identifier: String,
                   override val secrete: String,
                   override val scope: Option[List[String]],
                   authorizationDialogUrl: Uri,
                   accessTokenUrl: Uri,
                   resourceUrl: Uri,
                   redirectUrl:Uri
                  ) extends Issuer {

  override def authorizationDialog() : Uri = {
    val query = Uri.Query(Map(
      "client_id"-> this.identifier,
      "redirect_uri"-> redirectUrl.toString(),
      "scope" -> this.scope.map(_.mkString(",")).getOrElse(""),
      "response_type" -> "code"
    ))
    authorizationDialogUrl.withQuery(query)
  }

  override def accessTokenURL(q: Uri.Query) = {
    val query = Map(
      "client_id"-> this.identifier,
      "redirect_uri"-> redirectUrl.toString(),
      "client_secret" -> this.secrete,
      "grant_type" -> "authorization_code"
    )

    accessTokenUrl.withQuery(Uri.Query(query.++(q.toMap)))
  }

  override def resourceOwnerInfo(q: Uri.Query) = ???
}

