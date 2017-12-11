package model

/**
  *
  * @param id
  * @param providerID
  * @param providerKey
  */
case class LoginInfo(
                      id: Option[Long],
                      providerID: String,
                      providerKey: String
                    )


/**
  *
  * @param userID
  * @param loginInfoId
  */
case class UserLoginInfo(
                          userID: Long,
                          loginInfoId: Long
                        )


/**
  *
  * @param hasher
  * @param password
  * @param salt
  * @param loginInfoId
  */
case class PasswordInfo(
                         hasher: String,
                         password: String,
                         salt: Option[String],
                         loginInfoId: Long
                       )

/**
  *
  * @param id
  * @param token
  * @param secret
  * @param loginInfoId
  */
case class OAuth1Info(
                       id: Option[Long],
                       token: String,
                       secret: String,
                       loginInfoId: Long
                     )

/**
  *
  * @param id
  * @param accessToken
  * @param tokenType
  * @param expiresIn
  * @param refreshToken
  * @param loginInfoId
  */
case class OAuth2Info(
                       id: Option[Long],
                       accessToken: String,
                       tokenType: Option[String],
                       expiresIn: Option[Int],
                       refreshToken: Option[String],
                       loginInfoId: Long
                     )

/**
  *
  * @param id
  * @param loginInfoId
  */
case class OpenIDInfo(
                       id: String,
                       loginInfoId: Long
                     )

/**
  *
  * @param id
  * @param key
  * @param value
  */
case class OpenIDAttribute(
                            id: String,
                            key: String,
                            value: String
                          )

