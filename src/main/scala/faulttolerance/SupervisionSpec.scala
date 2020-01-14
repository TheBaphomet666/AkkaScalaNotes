package faulttolerance

import akka.actor.SupervisorStrategy.{Escalate, Restart, Resume, Stop}
import akka.actor.{Actor, ActorRef, ActorSystem, AllForOneStrategy, OneForOneStrategy, Props, SupervisorStrategy, Terminated}
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.wordspec.AnyWordSpecLike

class SupervisionSpec extends TestKit(ActorSystem("SupervisionSpec"))
with ImplicitSender
with AnyWordSpecLike
with BeforeAndAfterAll{

  override def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
  }
  import SupervisionSpec._

  "A supervisor" should {
    "resume it's child on minor fault" in {
      val supervisor = system.actorOf(Props[Supervisor])
      supervisor ! Props[FussyWordCounter]
      val child = expectMsgType[ActorRef]
      child ! "I love akka"
      child ! Report
      expectMsg(3)
      child ! "this is a very large sentence to be computed im so sorry"
      child ! Report
      expectMsg(3)
    }

    "restart it's child in case of an empty sentence" in {
      val supervisor = system.actorOf(Props[Supervisor])
      supervisor ! Props[FussyWordCounter]
      val child = expectMsgType[ActorRef]
      child ! "I love akka"
      child ! Report
      expectMsg(3)

      child ! ""
      child ! Report
      expectMsg(0)

    }

    "terminate it's child in case of a major error" in {
      val supervisor = system.actorOf(Props[Supervisor])
      supervisor ! Props[FussyWordCounter]
      val child = expectMsgType[ActorRef]
      watch(child)
      child ! "not uppercase"
      val terminatedMessage = expectMsgType[Terminated]
      assert(terminatedMessage.actor == child)
    }

    "Esacalate an error when it doesnt know what to do" in {
      val supervisor = system.actorOf(Props[Supervisor],"supervisor")
      supervisor ! Props[FussyWordCounter]
      val child = expectMsgType[ActorRef]
      watch(child)
      child ! 231
      val terminatedMessage = expectMsgType[Terminated]
      assert(terminatedMessage.actor == child)
    }
  }
  "A kinder supervisor" should {
    "not kill children in case it's escalates failures" in {
      val supervisor = system.actorOf(Props[NoDeathOnRestartSupervisor],"supervisor")
      supervisor ! Props[FussyWordCounter]
      val child = expectMsgType[ActorRef]

      child ! "Akka is cool"
      child ! Report
      expectMsg(3)

      child ! 43
      child ! Report
      expectMsg(0)

    }
  }
}

object SupervisionSpec {

  class Supervisor extends Actor {

    override val supervisorStrategy: SupervisorStrategy = OneForOneStrategy(){
      case _: NullPointerException => Restart
      case _: IllegalArgumentException => Stop
      case _: RuntimeException => Resume
      case _: Exception => Escalate
    }

    override def receive: Receive = {
      case props: Props =>
        val childRef = context.actorOf(props)
        sender() ! childRef
    }
  }

  class NoDeathOnRestartSupervisor extends Supervisor {
    override def preRestart(reason: Throwable, message: Option[Any]): Unit = {}
  }
  case object Report
  class FussyWordCounter extends Actor {
    var words= 0

    override def receive: Receive = {
      case Report=> sender() ! words
      case "" => throw new NullPointerException("Sentence is empty")
      case sentence: String =>
        if(sentence.length>20) throw new RuntimeException("Sentence is too big")
        else if (!Character.isUpperCase(sentence(0))) throw new IllegalArgumentException("Sentence must start with uppercase")
        else words += sentence.split(" ").length
      case _ => throw new Exception("can only receive Strings")
    }
  }

  class AllForOneSupervisor extends Supervisor {
    override val supervisorStrategy = AllForOneStrategy(){
      case _: NullPointerException => Restart
      case _: IllegalArgumentException => Stop
      case _: RuntimeException => Resume
      case _: Exception => Escalate
    }
  }


}
