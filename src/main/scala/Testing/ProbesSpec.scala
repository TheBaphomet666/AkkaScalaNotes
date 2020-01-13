package Testing

import akka.actor.AbstractActor.Receive
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.wordspec.AnyWordSpecLike

class ProbesSpec extends TestKit(ActorSystem("testprobespec"))
with ImplicitSender
with AnyWordSpecLike
with BeforeAndAfterAll{
  override def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
  }
  import TestProbeSpec._

  "A master actor" should{
    "register a slave" in {
      val master = system.actorOf(Props[Master])
      val slave = TestProbe("slave")

      master ! Register(slave.ref)
      expectMsg(RegistrationAck)
    }
    "send the work to the slave" in {
      val master = system.actorOf(Props[Master])
      val slave = TestProbe("slave")
      master ! Register(slave.ref)
      expectMsg(RegistrationAck)
      val workLoad = "I love akka"
      master ! Work(workLoad)

      slave.expectMsg(SlaveWork(workLoad,testActor))
      slave.reply(WorkCompleted(3,testActor))

      expectMsg(Report(3))
    }
    "Aggregate Data correctly" in {
      val master = system.actorOf(Props[Master])
      val slave = TestProbe("slave")
      master ! Register(slave.ref)
      expectMsg(RegistrationAck)

      val workLoad = "I love akka"
      master ! Work(workLoad)
      master ! Work(workLoad)

      slave.receiveWhile(){
        case SlaveWork(`workLoad`,`testActor`)=> slave.reply(WorkCompleted(3,testActor))
      }
      expectMsg(Report(3))
      expectMsg(Report(6))
    }
  }
}

object TestProbeSpec {

  case class Work(text : String)
  case class SlaveWork(text : String,requester: ActorRef)
  case class Register(slaveRef: ActorRef)
  case class WorkCompleted(count: Int, requester: ActorRef)
  case class Report(i: Int)
  case object RegistrationAck
  class Master extends Actor {
    override def receive: Receive = {
      case Register(slaveRef) =>
        sender() ! RegistrationAck
        context.become(online(slaveRef, 0))
      case _ =>
    }
    def online(slaveRef: ActorRef, totalwords: Int): Receive = {
      case Work(text)=> slaveRef ! SlaveWork(text,sender())
      case WorkCompleted(count, requester)=>
        val newtotalwords = totalwords + count
        requester ! Report(newtotalwords)
        context.become(online(slaveRef,newtotalwords))
    }
  }




}
