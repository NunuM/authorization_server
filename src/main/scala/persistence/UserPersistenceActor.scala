package persistence

import akka.actor.{Actor, ActorLogging}
import bootstrap.Tables
import model.Messages._
import model._
import org.mindrot.jbcrypt
import org.mindrot.jbcrypt.BCrypt
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
      Database.forConfig("h2mem1")
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
    case CreateUser(user,password) => {
      log.debug(s"creating user: $user")
      sender() ! create(user,password)
    }
    case LoginUser(username) => {
      log.debug(s"login user: $username")
      sender() ! login(username)
    }
    case OauthLoginUser(u,p,i) => {
      sender() ! createOrUpdate(u,p,i)
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

  private def login(username:String): Future[Option[(User,String,String)]] = {

    val actions = for {
      user <- this.users if user.username === username || user.email === username
      userLogin <- this.userLoginInfos if userLogin.userId === user.id
      passwordInfos <- this.passwordInfos if passwordInfos.loginInfoId === userLogin.loginInfoId
   } yield (user,passwordInfos.password,passwordInfos.salt.getOrElse(""))

    this.databaseConnection match {
      case Some(connection) => connection.run(actions.result.headOption)
      case _ => Future(None)
    }
  }

  private def createOrUpdate(user:User, preAuth: PreAuth, issuerCredential: IssuerCredential): Future[User] = {

    val dbLoginInfo = LoginInfo(None, issuerCredential.clientId, issuerCredential.clientSecrete)

    val loginInfoAction = {
      val retrieveLoginInfo = loginInfos.filter(info => info.providerId === issuerCredential.clientId && info.providerKey === issuerCredential.clientSecrete).result.headOption
      val insertLoginInfo = loginInfos.returning(loginInfos.map(_.id)).into((info, id) => info.copy(id = Some(id))) += dbLoginInfo
      for {
        loginInfoOption <- retrieveLoginInfo
        loginInfo <- loginInfoOption.map(DBIO.successful(_)).getOrElse(insertLoginInfo)
      } yield loginInfo
    }

    val actions = (for {
      _ <- users.insertOrUpdate(user)
      loginInfo <- loginInfoAction
      _ <- userLoginInfos += UserLoginInfo(user.id.get, loginInfo.id.get)
    } yield ()).transactionally

    this.databaseConnection.get.run(actions).map(_ => user)
  }


  private def create(user: User,password:String): Future[Option[Unit]] = {

    this.databaseConnection match {
      case Some(connection) => {
        val salt = BCrypt.gensalt()
        val passwordHashed = BCrypt.hashpw(password, salt)
        val actions = (for{

          id <- (users returning users.map(_.id)) += user
          passwordLoginInfo <- this.loginInfos.filter(_.providerId === "password").result.head
          _ <- userLoginInfos += UserLoginInfo(id,passwordLoginInfo.id.get)
          _ <- passwordInfos += PasswordInfo("bcrypt",passwordHashed,Option(salt.toString),id)

        } yield ()).transactionally

        connection.run(actions).map(Option(_))
      }
      case _ => Future(None)
    }
  }

}
