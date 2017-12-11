package model

object Messages {


  case class CreateUser(user: User,password:String)
  case class FindUser(username:String)
  case class LoginUser(username:String)
}
