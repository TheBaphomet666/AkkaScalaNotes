package faulttolerance

import java.io.File

import akka.actor.SupervisorStrategy.Stop
import akka.actor.{Actor, ActorLogging, ActorSystem, OneForOneStrategy, Props}
import akka.pattern.{Backoff, BackoffOpts, BackoffSupervisor}
import scala.language.postfixOps
import scala.concurrent.duration._
import scala.io.Source

object BackoffSupervisorPattern extends App {

  case object ReadFile

  class FileBasedPersistentActor extends Actor with ActorLogging {
    var dataSource: Source = null

    override def preRestart(reason: Throwable, message: Option[Any]): Unit =
      log.info("Persistent actor starting")

    override def postStop(): Unit =
      log.warning("Persistent actor has stopped")

    override def aroundPreRestart(reason: Throwable, message: Option[Any]): Unit =
      log.warning("per")
    override def receive: Receive = {
      case ReadFile=>
        if(dataSource == null)
          dataSource = Source.fromFile(new File("src/main/resources/testFiles/important.txt"))
        log.info("I've just read some IMPORTANT data: "+ dataSource.getLines().toList)
    }
  }

  val system = ActorSystem("BackOffSupervisionPattern")

  val simpleActor = system.actorOf(Props[FileBasedPersistentActor],"SimpleActr")
  simpleActor ! ReadFile
  val simpleSupervisorProps = BackoffSupervisor.props(
    BackoffOpts.onFailure(
      Props[FileBasedPersistentActor],
      "simpleBackoffActor",
      3 seconds,
      30 seconds,
      0.2)
  )

  val simpleBackOffSupervisor = system.actorOf(simpleSupervisorProps,"simpleSupervisor")
  simpleBackOffSupervisor ! ReadFile

  val stopSupervisorProps = BackoffSupervisor.props(
    BackoffOpts.onStop(
      Props[FileBasedPersistentActor],
      "stopBackoffActor",
      3 seconds,
      30 seconds,
      0.2
    ).withSupervisorStrategy(
      OneForOneStrategy() {
        case _ => Stop
      }
    )
  )
  val stopSupervisor = system.actorOf(stopSupervisorProps, "stopSupervisor")
  stopSupervisor ! ReadFile

  class EagerFBPActor extends FileBasedPersistentActor {
    override def preStart(): Unit = {
      log.info("Eager actor starting")
      dataSource = Source.fromFile(new File("src/main/resources/testFiles/important.txt"))
    }
  }

  val eagerFBPActor = system.actorOf(Props[EagerFBPActor])

  val repeatedSupervisorProps = BackoffSupervisor.props(
    BackoffOpts.onStop(
      Props[EagerFBPActor],
      "EagerActor",
      1 second,
      30 seconds,
      0.1
    )
  )

  val repeatedSupervisor = system.actorOf(repeatedSupervisorProps,"eagerSupervisor")
}
