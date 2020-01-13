package part5

import akka.actor.{Actor, ActorContext, ActorRef, ActorSystem, Props}

object part5 extends App{

  object Mom {
    case class MomStart(kid: ActorRef)
    case class Food(food: String)
    case class Ask(message: String)
    val VEGETABLE="yuck"
    val CHOCOLATE="yummy"
  }

  object FussyKid{
    case object Accept
    case object Reject
    val HAPPY=":)"
    val SAD=":("
  }


  class FussyKid extends Actor {
    import FussyKid._
    import Mom._
    var state = HAPPY
    override def receive: Receive = {
      case Food(VEGETABLE)=> state=SAD
      case Food(CHOCOLATE)=> state=HAPPY
      case Ask(_)=>
        if(state==HAPPY) sender() ! Accept
        else sender() ! Reject

    }
  }

  class StatelessKid extends Actor {
    import FussyKid._
    import Mom._
    override def receive: Receive = happyReceive

    def happyReceive: Receive= {
    case Food(VEGETABLE)=> context.become(sadReceive,false)
    case Food(CHOCOLATE)=> context.become(happyReceive,false)
    case Ask(_)=> sender() ! Accept
    }
    def sadReceive: Receive = {
      case Food(VEGETABLE)=> context.become(sadReceive,false)
      case Food(CHOCOLATE)=> context.unbecome()
      case Ask(_)=> sender() ! Reject
    }
  }

  class Mom extends Actor{
    import Mom._
    import FussyKid._
    override def receive: Receive = {
      case MomStart(kid)=>
        kid ! Food(VEGETABLE)
        kid ! Ask("play?")
      case Accept=> print("yay")
      case Reject=> print("Fuck him")
    }
  }

  import Mom._
  val system = ActorSystem("actsys")
  val fussyKid = system.actorOf(Props[FussyKid])
  val mom = system.actorOf(Props[Mom])
  val statelessKid = system.actorOf(Props[StatelessKid])
  mom ! MomStart(statelessKid)

  object Counter {

    case object Increment
    case object Decrement
    case object Print

  }

  class Counter extends Actor {
    import Counter._
    var count = 0
    override def receive: Receive = {
      case Increment => count +=1
      case Decrement => count -=1
      case Print=> print(s"the count is $count")
    }
  }

  case class Vote(candidate :String)
  case object VoteStatusRequest
  case class VoteStatusReply(candidate : Option[String])

  class Citizen extends Actor{
    var candidate: Option[String]= None
    override def receive: Receive ={
      case Vote(c) => candidate=Some(c)
      case VoteStatusRequest => sender() ! VoteStatusReply(candidate)
    }
  }

  case class GetVotes(citizens: Set[ActorRef])

  class Jury extends Actor{
    var stillWaiting: Set[ActorRef] = Set()
    var currentStats : Map[String,Int]= Map()
    override def receive: Receive ={
      case GetVotes(citizens)=>
        stillWaiting=citizens
        citizens.foreach(citizensRef => citizensRef ! VoteStatusRequest)
      case VoteStatusReply(None)=>
        sender() ! VoteStatusRequest
      case VoteStatusReply(Some(candidate))=>
        val newStillWaiting = stillWaiting - sender()
        val currentVotesOfCandidate = currentStats.getOrElse(candidate,0)
        currentStats= currentStats + (candidate-> (currentVotesOfCandidate+1))
        if(newStillWaiting.isEmpty){
          print(s"[Jury] poll stats: $currentStats")
        } else {
          stillWaiting = newStillWaiting
        }
    }
  }

  class StatelessCitizen extends Actor{
    override def receive: Receive ={
      case Vote(c) => context.become(voted(c))
      case VoteStatusRequest => sender() ! VoteStatusReply(None)
    }

    def voted(candidate : String): Receive = {
      case VoteStatusRequest=> sender() ! VoteStatusReply(Some(candidate))
    }
  }

  class StatelessJury extends Actor{

    override def receive: Receive = awaittingCommand

    def awaittingCommand : Receive = {
      case GetVotes(citizens)=>
        citizens.foreach(citizensRef => citizensRef ! VoteStatusRequest)
        context.become(awaitingStatuses(citizens, Map()))
    }
    def awaitingStatuses(stillWaiting: Set[ActorRef], currentStats: Map[String,Int]): Receive ={
      case VoteStatusReply(None)=>
        sender() ! VoteStatusRequest
      case VoteStatusReply(Some(candidate))=>
        val newStillWaiting = stillWaiting - sender()
        val currentVotesOfCandidate = currentStats.getOrElse(candidate,0)
        val newcurrentStats= currentStats + (candidate-> (currentVotesOfCandidate+1))
        if(newStillWaiting.isEmpty){
          print(s"[Jury] poll stats: $newcurrentStats")
        } else {
          context.become(awaitingStatuses(newStillWaiting,newcurrentStats))
        }
    }
  }

  val alice = system.actorOf(Props[StatelessCitizen])
  val jhon = system.actorOf(Props[StatelessCitizen])
  val mike = system.actorOf(Props[StatelessCitizen])
  val kurt = system.actorOf(Props[StatelessJury])

  alice ! Vote("Rick")
  jhon ! Vote ("Jack")
  mike ! Vote ("Jack")

  kurt ! GetVotes(Set(alice,jhon,mike))


}
