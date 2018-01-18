package model

trait AccessToken {
  type AccessToken  = String
  type RefreshToken = String
  type TokenType    = String
  type ValidUntil   = Long

  val accessToken : AccessToken
  val refreshToken : Option[RefreshToken]
  val tokenType : TokenType
  val validUntil : Option[ValidUntil]
}