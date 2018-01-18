package model

object Issuers extends Enumeration {
  type Issuers = Value
  val Facebook, Google = Value

  implicit def fromString(issuer: String): Option[Issuers] = {
    issuer match {
      case "facebook" => Some(Facebook)
      case "google" => Some(Google)
      case _ => None
    }
  }
}
