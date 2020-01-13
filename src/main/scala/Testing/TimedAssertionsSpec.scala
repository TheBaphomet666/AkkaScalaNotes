package Testing

import akka.actor.{Actor, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import com.typesafe.config.ConfigFactory
import org.scalatest.BeforeAndAfterAll
import org.scalatest.wordspec.AnyWordSpecLike

import scala.concurrent.duration._
import scala.util.Random
import scala.language.postfixOps

class TimedAssertionsSpec extends TestKit(ActorSystem("TimedAssertions", ConfigFactory.load().getConfig("specialTimedAssertionsConfig")))
  with ImplicitSender
  with AnyWordSpecLike
  with BeforeAndAfterAll {

  override def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
  }

  import TimedAssertionsSpec._

  "A Worker Actor" should {
    val workerActor = system.actorOf(Props[WorkerActor])
    "reply with bla bla" in {
      within(500 millis, 1 second){
        workerActor ! "work"
        expectMsg(WorkResult(42))
      }
    }

    "reply with valid work at reasonable cadence" in {
      within(1 second) {
        workerActor ! "workSequence"

        val results = receiveWhile[Int](2 seconds, 500 millis, 10) {
          case WorkResult(result) => result
        }
        assert(results.sum > 5)
      }
    }
      "reply to  test probe in a timely manner" in {
        within(1 second) {
          val probe = TestProbe()
          probe.send(workerActor, "work")
          probe.expectMsg(WorkResult(42))
        }
    }

  }
}

object TimedAssertionsSpec {

  case class WorkResult(result: Int)
  class WorkerActor extends Actor {
    override def receive: Receive = {
      case "work"=>
        Thread.sleep((500))
        sender ! WorkResult(42)
      case "workSequence"=>
        val r = new Random()
        for (i <- 1 to 10){
          Thread.sleep(r.nextInt(50))
          sender() ! WorkResult(1)
        }
    }
  }

}
