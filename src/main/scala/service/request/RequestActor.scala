package service.request

import akka.actor.{Actor, ActorLogging}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{HttpRequest, HttpResponse}
import akka.pattern.{CircuitBreaker, pipe}
import akka.stream.ActorMaterializer
import model.Messages.MakeRequest

import scala.concurrent.Future
import scala.concurrent.duration._

class RequestActor extends Actor with ActorLogging {


  implicit val system = context.system
  implicit val materializer = ActorMaterializer()

  import context.dispatcher

  val breaker =
    new CircuitBreaker(
      context.system.scheduler,
      maxFailures = 5,
      callTimeout = 30.seconds,
      resetTimeout = 1.minute).onOpen(notifyMeOnOpen())

  def notifyMeOnOpen(): Unit = log.warning("My CircuitBreaker is now open, and will not close for one minute")

  def dangerousCall: String = "This really isn't that dangerous of a call after all"

  override def receive: Receive = {
    case request: MakeRequest =>
     sender() ! breaker.withSyncCircuitBreaker(responseFuture(request.httpRequest))
    case "block for me" =>
      sender() ! breaker.withSyncCircuitBreaker(dangerousCall)
  }

  def responseFuture(httpRequest: HttpRequest): Future[HttpResponse] = {
    Http().singleRequest(httpRequest)
  }
}