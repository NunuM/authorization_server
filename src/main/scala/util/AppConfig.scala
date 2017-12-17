package util

import java.net.InetAddress

import akka.http.scaladsl.model.Uri
import com.typesafe.config.Config
import com.typesafe.config.{Config, ConfigFactory, ConfigObject}
import model.{FacebookIssuer, GoogleIssuer, Issuer}


final class AppConfig(conf: Option[Config] = None) {


  val localAddress: String = InetAddress.getLocalHost.getHostAddress

  val rootConfig: Config = conf match {
    case Some(c) => c.withFallback(ConfigFactory.load)
    case _ => ConfigFactory.load
  }

  protected val issuers: Config = rootConfig.getConfig("issuers")

  private lazy val facebook : Issuer = issuerInfo("facebook")
  private lazy val google : Issuer = issuerInfo("google")

  val issuer = (issuer:String) => issuer match {
    case "facebook" => Option(facebook)
    case "google" => Option(google)
    case _ => None
  }

  val facebookIssuer : Issuer = issuerInfo("facebook")
  val googleIssuer : Issuer = issuerInfo("google")

  private def issuerInfo(issuer: String): Issuer = {

    val identifier = issuers.getString(s"$issuer.identifier")
    val secrete = issuers.getString(s"$issuer.secret")
    val authorizationDialog = Uri(issuers.getString(s"$issuer.authorizationDialog"))
    val accessTokenURL = Uri(issuers.getString(s"$issuer.accessTokenURL"))
    val resourceOwnerInfo = Uri(issuers.getString(s"$issuer.resourceOwnerInfo"))
    val redirectURL = Uri(issuers.getString(s"$issuer.redirectURL"))
    val scope = Option(issuers.getString(s"$issuer.scope").split("\\,").toList)

    issuer match {
      case "facebook" => {
        new FacebookIssuer(identifier, secrete, scope, authorizationDialog, accessTokenURL, resourceOwnerInfo, redirectURL)
      }
      case "google" =>
        new GoogleIssuer(identifier, secrete, scope, authorizationDialog, accessTokenURL, resourceOwnerInfo, redirectURL)
    }
  }


}

