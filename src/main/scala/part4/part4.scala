package part4

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

object part4 extends App{

  val actorSystem = ActorSystem("AkkSystem")

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

  val counter = actorSystem.actorOf(Props(new Counter()))

  object BankActor{
    case class Deposit(value: Int)
    case class withdraw(value: Int)
    case object statement
    case class success(msg:String)
    case class failure(msg:String)

  }

  class BankActor extends Actor {
    import BankActor._
    var map = collection.mutable.Map[ActorRef, Int]()
    override def receive: Receive = {
      case Deposit(value)=>
        if (map.contains(context.sender()))
          map(context.sender())+= value
        else
          map(context.sender())= value

      case withdraw(value)=>
        if (map.contains(context.sender()))
          map(context.sender())-= value
        else
          map(context.sender())= value
      case statement=>
        val money = map(context.sender())
        print(s"Statement of the account is $money \n")
    }
  }

  object Holder{
    case class Deposit1(bank: ActorRef, amount : Int)
    case class withdraw1(bank: ActorRef, amount : Int)
    case class statement1(bank: ActorRef)

  }

  class Holder(name : String) extends Actor {

    import Holder._
    import BankActor._

    override def receive: Receive = {
      case Deposit1(bank: ActorRef, amount : Int) => bank ! Deposit(amount)
      case withdraw1(bank: ActorRef, amount : Int) => bank ! withdraw(amount)
      case statement1(bank: ActorRef)=>  bank ! statement
    }
  }

  val bank = actorSystem.actorOf(Props(new BankActor))
  val alice = actorSystem.actorOf(Props(new Holder("Alice")))
  val jhon = actorSystem.actorOf(Props(new Holder("Jhon")))
  import  Holder._
  alice ! Deposit1(bank,20)
  alice ! withdraw1(bank,5)
  alice ! statement1(bank)

  jhon ! Deposit1(bank,54)
  jhon ! withdraw1(bank,12)
  jhon ! statement1(bank)

}
