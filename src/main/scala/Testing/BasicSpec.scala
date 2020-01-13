package Testing

import scala.concurrent.duration._
import akka.actor.{Actor, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.wordspec.AnyWordSpecLike

import scala.util.Random
class BasicSpec extends TestKit(ActorSystem("BasicSpec"))
  with ImplicitSender
  with AnyWordSpecLike
  with BeforeAndAfterAll {

 override def afterAll(): Unit = {
   TestKit.shutdownActorSystem(system)
 }

  import BasicSpec._
  "A simple actor" should {
    "send back the same message" in {
      val echoActor = system.actorOf(Props[SimpleActor])
      val message = "hello test"
      echoActor ! message
      expectMsg(message)
    }
  }

  "A Black Hole" should {
    "send back the some message" in {
      val blackHole = system.actorOf(Props[BlackHole])
      val message = "hello test"
      blackHole ! message
      expectNoMessage()
    }
  }

  "A Lab Test Actor" should {
    val worker = system.actorOf(Props[LabTestActor])
    "turn string into uppercase" in {
      val message = "hello test"
      worker ! message
      //expectMsg("HELLO TEST")
      val reply = expectMsgType[String]
      assert(reply == "HELLO TEST")
    }
    "reply to a greeting" in {
      worker ! "greeting"
      expectMsgAnyOf("hi", "hello")
    }
    "reply with favorite" in {
      worker ! "favorite"
      expectMsgAllOf("scala","akka")
    }
    "reply with a cool in a diff way" in {
      worker ! "favorite"
      val messages = receiveN(2) // Seq[Any]
    }
    "reply with cool fancy way" in {
      worker ! "favorite"
      expectMsgPF(){
        case "scala"=>
        case "akka"=>
      }
    }
  }

}

object BasicSpec {

  class SimpleActor extends Actor {
    override def receive: Receive = {
      case msg => sender() ! msg
    }
  }
  class BlackHole extends Actor {
    override def receive: Receive = {
      case _ =>
    }
  }
  class LabTestActor extends Actor {
    val random = new Random()
    override def receive: Receive = {
      case "greeting"=>
        if(random.nextBoolean()) sender() ! "hi" else sender() ! "hello"
      case "favorite" =>
        sender() ! "scala"
        sender() ! "akka"
      case message: String => sender() ! message.toUpperCase

    }
  }
}