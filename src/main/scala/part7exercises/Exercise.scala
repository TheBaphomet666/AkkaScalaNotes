package part7exercises

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import part7exercises.Exercise.Boss.MakeWork

import scala.collection.mutable.ListBuffer

object Exercise extends App{


  object WordCounterMaster {
    case class Initialize(nChild: Int)
    case class WordCountTask(text:String)
    case class WordCountReply(count:Int)
  }
  class WordCounterMaster extends Actor{
    import WordCounterMaster._
    override def receive: Receive = {
      case Initialize(nChild)=>{
        val children = ListBuffer[ActorRef]()
        1 to nChild foreach(_ =>  children += context.actorOf(Props[WordCounterWorker]) )
        context.become(Working(children,0, Map()))
      }
    }
    def Working(workers: ListBuffer[ActorRef], currentWorker: Int, agenda: Map[ActorRef,ActorRef]) : Receive = {
      case WordCountTask(text)=>
        println(s"[Master] Putting to work $currentWorker with: $text")
        workers(currentWorker) !  text
        val newagenda = agenda + (workers(currentWorker)-> sender())
        context.become(Working(workers,(currentWorker+1) % workers.length,newagenda))
      case WordCountReply(count)=>
        agenda(sender()) ! count
    }
  }

  class WordCounterWorker extends Actor{
    import WordCounterMaster._
    override def receive: Receive = {
      case text=>
        println("[Worker] working like a slave")
        sender() ! WordCountReply(text.toString.split(" ").length)

    }
  }

  object Boss{
    case class MakeWork(master: ActorRef, text: String)
  }
  class Boss extends Actor {
    import Boss._
    import WordCounterMaster._
    override def receive: Receive = {
      case MakeWork(worker,text) => worker ! WordCountTask(text)
      case msg=> println(s"[BOSS ${self.path}] Received for my request $msg")
    }
  }

  val system = ActorSystem("system")
  val master = system.actorOf(Props[WordCounterMaster])
  val karl = system.actorOf(Props[Boss],"Karl")
  val mike = system.actorOf(Props[Boss],"mike")
  val ana = system.actorOf(Props[Boss],"ana")

  import WordCounterMaster._
  master ! Initialize(10)

  karl ! MakeWork(master,"Akka is Holy damn Awesome")
  mike ! MakeWork(master,"Akka is perry Awesome")
  ana ! MakeWork(master,"Akka is not at all i hate it Awesome")





}
