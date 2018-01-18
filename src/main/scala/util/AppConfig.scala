package util

import java.net.InetAddress

import akka.http.scaladsl.model.Uri
import com.typesafe.config.Config
import com.typesafe.config.{Config, ConfigFactory, ConfigObject}
import model.{FacebookOauthApplication, GoogleOauthApplication, Issuers, OauthApplication}

import scala.util.Try


final class AppConfig(conf: Option[Config] = None) {


  val localAddress: String = InetAddress.getLocalHost.getHostAddress

  val rootConfig: Config = conf match {
    case Some(c) => c.withFallback(ConfigFactory.load)
    case _ => ConfigFactory.load
  }


  private val issuers: Config = rootConfig.getConfig("issuers")
  private val facebookConfig = rootConfig.getConfig("issuers.facebook")
  private val googleConfig = rootConfig.getConfig("issuers.google")


  val googleOauth = oauthAppInfo(Issuers.Google)
  val facebookOauth = oauthAppInfo(Issuers.Facebook)

  def oauthAppInfo(issuer: Issuers.Value) = {
    issuer match {
      case Issuers.Google => {(googleConfig.getString("identifier"),
          googleConfig.getString("secret"),
          googleConfig.getString("authorizationDialog"),
          googleConfig.getString("accessTokenURL"),
          googleConfig.getString("resourceOwnerInfo"),
          googleConfig.getString("redirectURL"),
          googleConfig.getString(s"scope").split("\\,").toList)
      }
      case Issuers.Facebook => { (facebookConfig.getString("identifier"),
          facebookConfig.getString("secret"),
          facebookConfig.getString("authorizationDialog"),
          facebookConfig.getString("accessTokenURL"),
          facebookConfig.getString("resourceOwnerInfo"),
          facebookConfig.getString("redirectURL"),
          facebookConfig.getString(s"scope").split("\\,").toList)
      }
    }
  }

  /** Attempts to acquire from environment, then java system properties. */
  def withFallback[T](env: Try[T], key: String): Option[T] = env match {
    case null => None
    case value => value.toOption
  }

}

