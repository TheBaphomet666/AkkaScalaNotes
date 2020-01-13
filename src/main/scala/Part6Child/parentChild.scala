package Part6Child

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

object Part6Child extends App {

  object Parent {
    case class CreateChild(name: String)
    case class TellChild(message:String)
  }
  class Parent extends Actor {
    import Parent._
    import Child._

    override def receive: Receive = {
      case CreateChild(name)=>
        println(s"${self.path} creating child")
        val childRef = context.actorOf(Props[Child], name)
        context.become(withChild(childRef))

    }
    def withChild(ref: ActorRef) : Receive ={
      case TellChild(message)=>
        if(ref != null) ref forward Tell(message)
    }
  }

  object Child {
    case class Tell(message:String)
  }
  class Child extends Actor {
  import Child._
    override def receive: Receive = {
      case Tell(message) =>
        val gc = context.actorOf(Props[GrandChild],"Grando_Smokio")
        println(s"${self.path} sending to GrandChild: $message")
        gc ! message
    }
  }

  class GrandChild extends Actor {
    override def receive: Receive = {
      case msg => println(s"${self.path} I got: $msg")
    }
  }

  import Parent._
  val system = ActorSystem("parentChildDemo")
  val parent = system.actorOf(Props[Parent], "parent")
  parent ! CreateChild("big_Child")
  parent ! TellChild("hey kid")

  val childSelection = system.actorSelection("/user/parent/big_Child/Grando_Smokio")
  childSelection ! "Found you"



}
