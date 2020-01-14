package Infra

import akka.actor.AbstractActor.Receive
import akka.actor.{Actor, ActorLogging, ActorSystem, Cancellable, PoisonPill, Props, Timers}
import com.sun.jdi.connect.VMStartException

import scala.concurrent.duration._
import scala.language.postfixOps
object TimeSchedulers extends App {

  class SimpleActor extends Actor with ActorLogging{
    override def receive: Receive = {
      case message=> log.info(message.toString)
    }
  }

  val system = ActorSystem("SchedulersTimeDemo")
  val simpleActor = system.actorOf(Props[SimpleActor])

  system.log.info("Scheduling reminder for simpleActor")

  import system.dispatcher

  system.scheduler.scheduleOnce(1 second){
    simpleActor ! "reminder"
  }

  val routine: Cancellable = system.scheduler.schedule(1 second,2 seconds){
    simpleActor ! "heartBeat"
  }

  system.scheduler.scheduleOnce(5 seconds){
    routine.cancel()
  }


  class Kamikaze extends Actor with ActorLogging {
    override def postStop(): Unit = log.info("I have stopped")
    override def receive: Receive = awaitingExplosion
    var death: Cancellable = null
    def awaitingExplosion: Receive = {
      case _ =>
        death = context.system.scheduler.scheduleOnce(1 second){
          context.stop(self)
        }
        log.info("exploding in one second")
        context.become(explodingInOneSecond)

    }
    def explodingInOneSecond: Receive = {
      case _ =>
        death.cancel()
        death = context.system.scheduler.scheduleOnce(1 second){
          context.stop(self)
        }
        log.info("Im safe for other 1 second")
    }
  }
  val kamizake = system.actorOf(Props[Kamikaze],"kamizake")
  val death: Cancellable = system.scheduler.schedule(1 second,1 seconds){
    kamizake ! "Die my friend"
  }
  system.scheduler.scheduleOnce(10 seconds){
    death.cancel()
  }

  case object TimerKey
  case object Start
  case object Reminder
  case object Stop
  class TimerBasedHeartBeatActor extends Actor with ActorLogging with Timers  {
    timers.startSingleTimer(TimerKey, Start, 500 millis)

    override def receive: Receive = {
      case Start=>
        log.info("BootStrapping")
        timers.startTimerAtFixedRate(TimerKey, Reminder,1 second)
      case Reminder =>
        log.info("I am alive")
      case Stop =>
        log.warning("Stopping")
        timers.cancel(TimerKey)
        context.stop(self)
    }

    val timerBasedHeartBeatActor = system.actorOf(Props[TimerBasedHeartBeatActor],"timerActor")
    system.scheduler.scheduleOnce(5 seconds) {
      timerBasedHeartBeatActor ! Stop
    }
  }



}
