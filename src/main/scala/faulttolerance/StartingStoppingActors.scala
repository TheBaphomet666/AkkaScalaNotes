package faulttolerance

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Kill, PoisonPill, Props, Terminated}

object StartingStoppingActors extends App {

  val system = ActorSystem("StoppingActorsDemo")

  object Parent {
    case class StartChild(name:String)
    case class StopChild(name : String)
    case object Stop
  }
  class Parent extends Actor with ActorLogging {
    import Parent._

    override def receive: Receive = withChildren(Map())

    def withChildren(children: Map[String,ActorRef]) : Receive = {
      case StartChild(name)=>
        log.info(s"Starting child $name")
        context.become(withChildren(children + (name-> context.actorOf(Props[Child],name))))
      case StopChild(name)=>
        log.info(s"Stopping child with name $name")
        val childOption = children.get(name)
        childOption.foreach(childref => context.stop(childref))
      case Stop=>
        log.info("stopping myself")
        context.stop(self)
      case message => log.info(message.toString)
    }
  }

  class Child extends Actor with ActorLogging {
    override def receive: Receive = {
      case message => log.info(message.toString)
    }
  }

  import Parent._

  val parent = system.actorOf(Props[Parent],"parent")
  parent ! StartChild("child1")
  val child = system.actorSelection("/user/parent/child1")
  child ! "hi kid!"
  parent ! StopChild("child1")
  //for(_ <- 1 to 10) child ! "are you there?"
  parent ! StartChild("child2")
  val child2 = system.actorSelection("/user/parent/child2")
  child2 ! "hi second child"
  parent ! Stop

  val looseActor = system.actorOf(Props[Child])
  looseActor ! "hello loose actor"
  looseActor ! PoisonPill
  looseActor ! "are you live? hehe"

  val abruptlyTerminateActor = system.actorOf(Props[Child])
  abruptlyTerminateActor ! "now die!!"
  abruptlyTerminateActor ! Kill
  abruptlyTerminateActor ! "u ok?"

  class Watcher extends Actor with ActorLogging {
    import Parent._
    override def receive: Receive = {
      case StartChild(name)=>
        val child = context.actorOf(Props[Child],name)
        log.info(s"Started and watching child $name")
        context.watch(child)
      case Terminated(ref)=>
        log.info(s"the reference im watching $ref has been stopped")
    }
  }

  val watcher = system.actorOf(Props[Watcher],"watcher")
  watcher ! StartChild("watchedChild")
  val watchedChild = system.actorSelection("/user/watcher/watchedChild")
  Thread.sleep(500)
  watchedChild ! PoisonPill
}
