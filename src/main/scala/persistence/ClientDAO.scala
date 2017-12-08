package persistence

import model.Client

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object ClientDAO {

  var clients : scala.collection.immutable.HashMap[String, Client] = scala.collection.immutable.HashMap[String,Client]()

  def create(client:Client): Future[Client] = Future{
    this.clients ++ Map(client.clientID -> client)
    client
  }

}
