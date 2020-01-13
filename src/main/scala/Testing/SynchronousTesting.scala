package Testing

import Testing.SynchronousTesting.{Counter, Inc, Read}
import akka.actor.{Actor, ActorSystem, Props}
import akka.testkit.{CallingThreadDispatcher, TestActorRef, TestProbe}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.wordspec.AnyWordSpecLike

import scala.concurrent.duration.Duration

class SynchronousTesting extends AnyWordSpecLike
with BeforeAndAfterAll{

  implicit val system = ActorSystem("SynchronousTestingSpec")

  override def afterAll(): Unit = {
    system.terminate()
  }

  "A counter" should{
    "synchornously increase it's counter" in {
      val counter = TestActorRef[Counter](Props[Counter])
      counter ! Inc
      assert(counter.underlyingActor.counter == 1)
    }
    "synchronously increase it's counter at the call of the receive function" in {
      val counter = TestActorRef[Counter](Props[Counter])
      counter.receive(Inc)
      assert(counter.underlyingActor.counter == 1)
    }
    "work on the calling thread dispatcher" in {
      val counter = system.actorOf(Props[Counter].withDispatcher(CallingThreadDispatcher.Id))
      val probe = TestProbe()
      probe.send(counter, Read)
      probe.expectMsg(Duration.Zero, 0)
    }

  }
}

object SynchronousTesting {
  case object Inc
  case object Read
  class Counter extends Actor {
    var counter = 0
    override def receive: Receive = {
      case Inc => counter +=1
      case Read => sender() ! counter
    }

  }
}
