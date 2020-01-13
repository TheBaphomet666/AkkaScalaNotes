package Logging

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import akka.event.Logging

object logging extends App {

  class ActorExplicitLogging extends Actor {
    val logger = Logging(context.system,this)
    override def receive: Receive = {
      case message => logger.info(message.toString)
    }
  }

  val system = ActorSystem("loggingDemo")
  val logger = system.actorOf(Props[ActorExplicitLogging])

  logger ! "Im logging explicit"

  class ActorWithLogging extends Actor with ActorLogging {
    override def receive: Receive = {
      case (a,b)=> log.info("val {} other {}",a,b)
      case msg => log.info(msg.toString)
    }
  }

  val impllog= system.actorOf(Props[ActorWithLogging])

  impllog ! "Im implicit logging"
  impllog ! (4,5)
}
