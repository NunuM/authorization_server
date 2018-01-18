package presentation

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import bootstrap.Tables
import model.FacebookOauthApplication
import service.AuthorizationServer
import util.AppConfig

import scala.concurrent.Await
import scala.concurrent.duration._


object MainApplication {
  def main(args: Array[String]): Unit = {

    if(args.length < 2){
      println("Usage: host port")
      System.exit(0)
    }

    println(Await.result(Tables.createTables(),Duration.Inf))


    val hostname = args(0)
    val port = args(1).toInt

    implicit val system = ActorSystem("security")

    implicit val config = new AppConfig()
    implicit val mat = ActorMaterializer()

    new AuthorizationServer(hostname,port)

  }
}
