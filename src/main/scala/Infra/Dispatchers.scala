package Infra

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import com.typesafe.config.ConfigFactory

import scala.concurrent.{ExecutionContext, Future}

object Dispatchers extends App {

  class Counter extends Actor with ActorLogging {
    var count= 0

    override def receive: Receive = {
      case message =>
        count +=1
        log.info(s"[$count] $message")
    }
  }

  //val system = ActorSystem("DispatchersDemo", ConfigFactory.load().getConfig("dispatchersDemo"))
  val system = ActorSystem("DispatchersDemo")
  val simpleCounterActors = for (i <- 1 to 10) yield system.actorOf(Props[Counter].withDispatcher("my-dispatcher"))

  class DBActor extends Actor with ActorLogging {
    implicit val executionContext : ExecutionContext = context.dispatcher

    override def receive: Receive = {
      case message => Future {
        Thread.sleep(5000)
        log.info(s"Success: $message")
      }
    }
  }

  val DBActor = system.actorOf(Props[DBActor])
  DBActor ! "lola lalo lula lelo"

  val nonblockingActor = system.actorOf(Props[Counter])

  for (i <- 1 to 1000) {
    val message = s"important message $i"
    DBActor ! message
    nonblockingActor ! message
  }


}
