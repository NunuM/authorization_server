package util

import java.net.InetAddress

import akka.http.scaladsl.model.Uri
import com.typesafe.config.Config
import com.typesafe.config.{Config, ConfigFactory, ConfigObject}
import model.{FacebookIssuerApplication, GoogleIssuerApplication, IssuerApplication}

import scala.util.Try


final class AppConfig(conf: Option[Config] = None) {


  val localAddress: String = InetAddress.getLocalHost.getHostAddress

  val rootConfig: Config = conf match {
    case Some(c) => c.withFallback(ConfigFactory.load)
    case _ => ConfigFactory.load
  }

  object Issuers extends Enumeration {
    type Issuers = Value
    val Facebook, Google = Value

    implicit def fromString(issuer: String): Issuers = {
      issuer match {
        case "facebook" => Facebook
        case "google" => Google
      }
    }
  }


  protected val issuers: Config = rootConfig.getConfig("issuers")
  private val facebookConfig = issuers.getConfig("facebook")
  private val googleConfig = issuers.getConfig("google")


  import Issuers._

  val googleOauth = oauthAppInfo(Issuers.Google)
  val facebookOauth = oauthAppInfo(Issuers.Facebook)

  def oauthAppInfo(issuer: Issuers) = {
    issuers match {
      case Google => {
        (
          googleConfig.getString("identifier"),
          googleConfig.getString("secret"),
          googleConfig.getString("authorizationDialog"),
          googleConfig.getString("accessTokenURL"),
          googleConfig.getString("resourceOwnerInfo"),
          googleConfig.getString("redirectURL"),
          googleConfig.getString(s"$issuer.scope").split("\\,").toList
        )
      }
      case Facebook => {
        (
          googleConfig.getString("identifier"),
          googleConfig.getString("secret"),
          googleConfig.getString("authorizationDialog"),
          googleConfig.getString("accessTokenURL"),
          googleConfig.getString("resourceOwnerInfo"),
          googleConfig.getString("redirectURL"),
          googleConfig.getString(s"$issuer.scope").split("\\,").toList
        )
      }
    }
  }

  /** Attempts to acquire from environment, then java system properties. */
  def withFallback[T](env: Try[T], key: String): Option[T] = env match {
    case null => None
    case value => value.toOption
  }

}

