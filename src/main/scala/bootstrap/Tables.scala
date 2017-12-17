package bootstrap

import model._
import slick.jdbc.H2Profile.api._
import slick.jdbc.meta.MTable
import slick.lifted.PrimaryKey

import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration

object Tables {

  private val users = TableQuery[Users]
  private val loginInfos = TableQuery[LoginInfos]
  private val userLoginInfos = TableQuery[UserLoginInfos]
  private val passwordInfos = TableQuery[PasswordInfos]
  private val oAuth2Infos = TableQuery[OAuth2Infos]

  def createTables(): Future[Any] = {
    val connection = Database.forConfig("mydb")

    val hasCreated = MTable.getTables map (tables => {!tables.exists(_.name.name == users.baseTableRow.tableName)})

    connection.run(hasCreated).map[Any]{
      case true => {
        val setUp = DBIO.seq((
          users.schema
            ++ loginInfos.schema
            ++ userLoginInfos.schema
            ++ passwordInfos.schema
            ++ oAuth2Infos.schema
          ).create,

          loginInfos += LoginInfo(None,"password","password"),
          loginInfos += LoginInfo(None,"167287979969308","d9f6c5d384487054aaebb7500793725b")

        )
        Await.result(connection.run(setUp),Duration.Inf)
      }
      case _ => {
        val actions = for{
          _ <- userLoginInfos.delete
          _ <- passwordInfos.delete
          _ <- users.delete
          _ <- oAuth2Infos.delete
        } yield ()

        Await.result(connection.run(actions),Duration.Inf)
      }
    }

  }

  class Users(tag: Tag) extends Table[User](tag, "USER") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)

    def username = column[String]("username", O.Unique)

    def firsName = column[String]("firstname")

    def email = column[String]("email", O.Unique)

    def isActive = column[Boolean]("isActive")

    override def * = (id.?, username, firsName, email, isActive) <> (User.tupled, User.unapply)
  }


  class LoginInfos(tag: Tag) extends Table[LoginInfo](tag, "LOGININFO") {

    def id = column[Long]("ID", O.PrimaryKey, O.AutoInc)

    def providerId = column[String]("PROVIDERID")

    def providerKey = column[String]("PROVIDERKEY")

    def * = (id.?, providerId, providerKey) <> (LoginInfo.tupled, LoginInfo.unapply)

  }

  class UserLoginInfos(tag: Tag) extends Table[UserLoginInfo](tag, "USERLOGININFO") {

    def id = column[Long]("ID", O.AutoInc)

    def userId = column[Long]("USER_ID")

    //def user = foreignKey("USER_LOGIN_FK", userId, users)(_.id)

    def loginInfoId = column[Long]("LOGININFO_ID")

    //def loginInfo = foreignKey("LOGININFO_LOGIN_FK", loginInfoId, loginInfos)(_.id)

    def pk = primaryKey("userId_loginInfoId",(userId,loginInfoId))

    def * = (id.?,userId, loginInfoId) <> (UserLoginInfo.tupled, UserLoginInfo.unapply)
  }

  class PasswordInfos(tag: Tag) extends Table[PasswordInfo](tag, "PASSWORDINFO") {

    def hasher = column[String]("HASHER")

    def password = column[String]("PASSWORD")

    def salt = column[Option[String]]("SALT")

    def loginInfoId = column[Long]("LOGININFO_ID")

    def userLoginInfo = foreignKey("PasswordInfos_LOGININFO_PASSWORD_FK", loginInfoId, userLoginInfos)(_.id)

    def * = (hasher, password, salt, loginInfoId) <> (PasswordInfo.tupled, PasswordInfo.unapply)
  }

  class OAuth2Infos(tag: Tag) extends Table[OAuth2Info](tag, "OAUTH2INFO") {

    def id = column[Long]("ID", O.PrimaryKey, O.AutoInc)

    def accessToken = column[String]("ACCESSTOKEN")

    def tokenType = column[Option[String]]("TOKENTYPE")

    def expiresIn = column[Option[Int]]("EXPIRESIN")

    def refreshToken = column[Option[String]]("REFRESHTOKEN")

    def loginInfoId = column[Long]("LOGININFO_ID")

    def userLoginInfo = foreignKey("OAuth2Infos_LOGININFO_PASSWORD_FK", loginInfoId, userLoginInfos)(_.id)

    def * = (id.?, accessToken, tokenType, expiresIn, refreshToken, loginInfoId) <> (OAuth2Info.tupled, OAuth2Info.unapply)
  }

}