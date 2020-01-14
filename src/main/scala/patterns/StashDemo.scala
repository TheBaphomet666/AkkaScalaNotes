package patterns

import akka.actor.{Actor, ActorLogging, ActorSystem, Props, Stash}

object StashDemo extends App {

  case object Open
  case object Close
  case object Read
  case class Write(data : String)

  class ResourceActor extends Actor with ActorLogging with Stash {
    private var innerData: String = ""
    override def receive: Receive = closed
    def closed: Receive= {
      case Open =>
        log.info("opening")
        unstashAll()
        context.become(open)
      case message =>
        log.info(s"stashing $message since i can't process it while closed")
        stash()
    }

    def open: Receive = {
      case Read =>
        log.info(s"I have read $innerData")
      case Write(data) =>
        log.info(s"I am writing $data")
        innerData= data
      case Close=>
        log.info("closing")
        unstashAll()
        context.become(closed)
      case message=>
        stash()
    }
  }

  val system= ActorSystem("StashDemo")
  val resourceActor = system.actorOf(Props[ResourceActor])

  resourceActor ! Read
  resourceActor ! Open
  resourceActor ! Open
  resourceActor ! Write("I love stash")
  resourceActor ! Close
  resourceActor ! Read
}
