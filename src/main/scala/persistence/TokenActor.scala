package persistence

import java.util.UUID
import java.util.concurrent.TimeUnit

import akka.actor.{Actor, ActorLogging}
import model.Messages.{Authenticate, BearerToken, GenerateToken, UpdateToken}

import scala.concurrent.duration.Duration
import scala.concurrent.duration._

class TokenActor extends Actor with ActorLogging{

  implicit val executionContext = context.dispatcher

  private var tokenManager = scala.collection.immutable.HashMap[UUID,(model.User,Long)]()

  override def preStart(): Unit = {
    this.context.system.scheduler.schedule(FiniteDuration(5l,TimeUnit.MINUTES),FiniteDuration(1l,TimeUnit.HOURS),self,UpdateToken)
    super.preStart()
  }

  override def receive: Receive = {
    case GenerateToken(user) => {
      println(user)
      val token = UUID.randomUUID()
      val expires = (2 day).toMillis + System.currentTimeMillis()
      tokenManager += (token -> (user, (2 day).toMillis + System.currentTimeMillis()))
      sender() ! BearerToken("bearer",token.toString ,expires.toString)
    }
    case Authenticate(token) => {
     sender() ! (tokenManager.get(token) match {
        case Some(u) if System.currentTimeMillis() > u._2 => Option(u)
        case _ => None
      })
    }
    case UpdateToken => tokenManager = tokenManager.filterNot(_._2._2 > System.currentTimeMillis())
  }
}
