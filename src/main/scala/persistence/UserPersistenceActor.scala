package persistence

import akka.actor.{Actor, ActorLogging}
import bootstrap.Tables
import model.Messages._
import model._
import org.mindrot.jbcrypt
import org.mindrot.jbcrypt.BCrypt
import slick.jdbc.H2Profile
import slick.jdbc.H2Profile.api._

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

class UserPersistenceActor extends Actor with ActorLogging {

  implicit val executionContext = context.dispatcher

  private val users = TableQuery[Tables.Users]
  private val loginInfos = TableQuery[Tables.LoginInfos]
  private val passwordInfos = TableQuery[Tables.PasswordInfos]
  private val userLoginInfos = TableQuery[Tables.UserLoginInfos]
  private val oAuth2Infos = TableQuery[Tables.OAuth2Infos]
  private var databaseConnection: Option[Database] = None


  override def preStart(): Unit = {

    log.info("PreStart")

    this.databaseConnection = Try {
      Database.forConfig("mydb")
    } match {
      case Success(connection) => Option(connection)
      case Failure(e) => {
        log.error("Cannot create database connection", e)
        None
      }
    }

    super.preStart()
  }

  override def postStop(): Unit = {
    super.postStop()
    this.databaseConnection.foreach(_.close())
  }

  override def receive = {
    case FindUser(username) => {
      log.debug(s"finding user: $username")
      sender() ! find(username)
    }
    case CreateUser(user, password) => {
      log.debug(s"creating user: $user")
      sender() ! create(user, password)
    }
    case LoginUser(username) => {
      log.debug(s"login user: $username")
      sender() ! login(username)
    }
    case OauthLoginUser(u, p, i) => {
      sender() ! createOrUpdate(u, p, i)
    }
  }

  private def find(username: String): Future[Option[User]] = {
    val query = this.users.filter(_.username === username).result.headOption.map {
      case Some(user) => Option(user)
      case _ => None
    }

    this.databaseConnection match {
      case Some(connection) => connection.run(query)
      case _ => Future(None)
    }
  }

  private def login(username: String): Future[Option[(User, String, String)]] = {

    val actions = for {
      user <- this.users if user.username === username || user.email === username
      userLogin <- this.userLoginInfos if userLogin.userId === user.id
      passwordInfos <- this.passwordInfos if passwordInfos.loginInfoId === userLogin.loginInfoId
    } yield (user, passwordInfos.password, passwordInfos.salt.getOrElse(""))

    this.databaseConnection match {
      case Some(connection) => connection.run(actions.result.headOption)
      case _ => Future(None)
    }
  }

  private def createOrUpdate(user: User, preAuth: PreAuth, issuerCredential: IssuerApplication): Future[User] = {

    val dbLoginInfo = LoginInfo(None, issuerCredential.identifier, issuerCredential.secrete)

    val loginInfoAction = {
      val retrieveLoginInfo = loginInfos.filter(
        info => info.providerId === issuerCredential.identifier &&
          info.providerKey === issuerCredential.secrete).result.headOption
      val insertLoginInfo = loginInfos.returning(loginInfos.map(_.id)).
        into((info, id) => info.copy(id = Some(id))) += dbLoginInfo

      for {
        loginInfoOption <- retrieveLoginInfo
        loginInfo <- loginInfoOption.map(DBIO.successful(_)).getOrElse(insertLoginInfo)
      } yield loginInfo
    }

    val actions = (for {
      user <- users.filter(_.email === user.email).result.headOption.flatMap {
        case Some(user) => DBIO.successful(user).map(_.id)
        case None => ((users returning users.map(user => user.id)) += user).map(Option(_))
      }

      loginInfo <- loginInfoAction
      userLoginInfo <- userLoginInfos.filter(t => t.userId === user.get && t.loginInfoId === loginInfo.id.get).result.headOption.flatMap {
        case Some(userLoginInfo) => DBIO.successful(userLoginInfo).map(_.id)
        case None => ((userLoginInfos returning userLoginInfos.map(loginInfo => loginInfo.id)) += UserLoginInfo(None, user.get, loginInfo.id.get)).map(Option(_))
      }
      _ <- oAuth2Infos.filter(_.loginInfoId === userLoginInfo.get).result.headOption.flatMap {
        case Some(o) => oAuth2Infos.update(OAuth2Info(o.id, preAuth.authToken, Option(preAuth.tokenType), Option(preAuth.expiresIn.toInt), None, userLoginInfo.get))
        case None => oAuth2Infos += OAuth2Info(None, preAuth.authToken, Option(preAuth.tokenType), Option(preAuth.expiresIn.toInt), None, userLoginInfo.get)
      }
    } yield ()).transactionally


    this.databaseConnection.get.run(actions).map(_ => user)
  }

  private def create(user: User, password: String): Future[Option[Unit]] = {

    this.databaseConnection match {
      case Some(connection) => {
        val salt = BCrypt.gensalt()
        val passwordHashed = BCrypt.hashpw(password, salt)
        val actions = (for {

          userId <- (users returning users.map(_.id)) += user
          passwordLoginInfo <- this.loginInfos.filter(_.providerId === "password").result.head
          userInfoId <- (userLoginInfos returning userLoginInfos.map(_.id)) += UserLoginInfo(None, userId, passwordLoginInfo.id.get)
          _ <- passwordInfos += PasswordInfo("bcrypt", passwordHashed, Option(salt.toString), userId)
        } yield ()).transactionally

        connection.run(actions).map(Option(_))
      }
      case _ => Future(None)
    }
  }

}
