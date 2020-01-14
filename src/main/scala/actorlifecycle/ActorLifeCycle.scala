package actorlifecycle

import akka.actor.{Actor, ActorLogging, ActorSystem, PoisonPill, Props}

object ActorLifeCycle extends App{

  case object StartChild
  class LifecycleActor extends Actor with ActorLogging {
    override def preStart(): Unit = log.info("I am Starting")
    override def postStop(): Unit = log.info("I have stopped")

    override def receive: Receive = {
      case StartChild =>
        context.actorOf(Props[LifecycleActor],"child")
    }
  }
  val system = ActorSystem("lifeCycleDemo")
  val parent = system.actorOf(Props[LifecycleActor],"parent")
  parent ! StartChild
  parent ! PoisonPill

  object  Fail
  object FailChild
  object CheckChild
  object Check
  class Parent extends Actor {
    val child = context.actorOf(Props[Child],"supervisedChild")
    override def receive: Receive = {
      case FailChild => child ! Fail
      case CheckChild => child ! Check
    }
  }
  class  Child extends Actor with ActorLogging {

    override def preStart(): Unit = log.info("supervisedChild started")

    override def postStop(): Unit = log.info("supervisedChild stopped")

    override def preRestart(reason: Throwable, message: Option[Any]): Unit =
      log.info(s"supervides actor restarting because of ${reason.getMessage}")

    override def postRestart(reason: Throwable): Unit =
      log.info("supervised actor restarted")
    override def receive: Receive = {
      case Fail=>
        log.warning("Child wil fail now")
        throw new RuntimeException("i failed you")
      case Check =>
        log.info("alive and kicking")
    }
  }

  val supervisor = system.actorOf(Props[Parent],"supervisor")
  supervisor ! FailChild
  supervisor ! CheckChild
}
