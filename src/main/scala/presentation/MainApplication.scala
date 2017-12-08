package presentation

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import service.AuthorizationServer;

object MainApplication {
  def main(args: Array[String]): Unit = {

    if(args.length < 2){
      println("Usage: host port")
      System.exit(0)
    }


    val hostname = args(0)
    val port = args(1).toInt

    implicit val system = ActorSystem("security")
    implicit val materializer = ActorMaterializer()
    implicit val executionContext = system.dispatcher


    AuthorizationServer.startServer(host = hostname,port,system)

  }
}
