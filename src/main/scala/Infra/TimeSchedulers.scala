package Infra

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import scala.concurrent.duration._
import scala.language.postfixOps
object TimeSchedulers extends App {

  class SimpleActor extends Actor with ActorLogging{
    override def receive: Receive = {
      case message=> log.info(message.toString)
    }
  }

  val system = ActorSystem("SchedulersTimeDemo")
  val simpleActor = system.actorOf(Props[SimpleActor])

  system.log.info("Scheduling reminder for simpleActor")

  system.scheduler.scheduleOnce(1 second){
    simpleActor ! "reminder"
  }(system.dispatcher)

}
