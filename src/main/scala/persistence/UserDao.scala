package persistence

import model.User

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global


object UserDao {
  var clients : scala.collection.immutable.HashMap[String, User] = scala.collection.immutable.HashMap[String,User]()

  def create(user:User): Future[User] = Future{
    this.clients ++ Map(user.username -> user.password)
    user
  }

  def find(username:String): Option[User] = {
    this.clients.get(username)
  }

}
