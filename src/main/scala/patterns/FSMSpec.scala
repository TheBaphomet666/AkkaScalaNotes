package patterns

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Cancellable, FSM, Props}
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.wordspec.AnyWordSpecLike

import scala.language.postfixOps
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class FSMSpec extends TestKit(ActorSystem("FSMSpec"))
with ImplicitSender
with AnyWordSpecLike
with BeforeAndAfterAll {

  override def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
  }

  import FSMSpec._

  "A vending machine " should {
    "Error when not initialized" in {
      val vendingMachine = system.actorOf(Props[VendingMachine])
      vendingMachine ! RequestProduct("Cocaine")
      expectMsg(VendingError("MachineNotInitialized"))
    }
    "Report a product not available" in {
      val vendingMachine = system.actorOf(Props[VendingMachine])
      vendingMachine ! Initialize(Map("cocaine" -> 0), Map("cocaine" -> 1))
      vendingMachine ! RequestProduct("crack")
      expectMsg(VendingError("ProductNotAvailable"))
    }
    "If i do no inser money" in {
      val vendingMachine = system.actorOf(Props[VendingMachine])
      vendingMachine ! Initialize(Map("cocaine" -> 1), Map("cocaine" -> 1))
      vendingMachine ! RequestProduct("cocaine")
      expectMsg(Instruction("Please insert 1"))
      within(1.5 seconds) {
        expectMsg(VendingError("Timeout"))
      }
    }
    "Handle reception of partial Money" in {
      val vendingMachine = system.actorOf(Props[VendingMachine])
      vendingMachine ! Initialize(Map("cocaine" -> 1), Map("cocaine" -> 3))
      vendingMachine ! RequestProduct("cocaine")
      expectMsg(Instruction("Please insert 3"))
      vendingMachine ! ReceiveMoney(1)
      expectMsg(Instruction("Please Insert 2"))
      within(2 seconds) {
        expectMsg(VendingError("Timeout"))
        expectMsg(GiveBackChange(1))
      }
    }
    "Deliver the product if i inserts all the money" in {
      val vendingMachine = system.actorOf(Props[VendingMachine])
      vendingMachine ! Initialize(Map("cocaine" -> 1), Map("cocaine" -> 3))
      vendingMachine ! RequestProduct("cocaine")
      expectMsg(Instruction("Please insert 3"))
      vendingMachine ! ReceiveMoney(3)
      expectMsg(Deliver("cocaine"))
    }
    "GiveBack change and be able to request money for a new product" in {
      val vendingMachine = system.actorOf(Props[VendingMachine])
      vendingMachine ! Initialize(Map("cocaine" -> 1), Map("cocaine" -> 3))
      vendingMachine ! RequestProduct("cocaine")
      expectMsg(Instruction("Please insert 3"))
      vendingMachine ! ReceiveMoney(4)
      expectMsg(Deliver("cocaine"))
      expectMsg(GiveBackChange(1))

    }

    "A VendingMachineFS " should {
      "Error when not initialized" in {
        val vendingMachine = system.actorOf(Props[VendingMachineFS])
        vendingMachine ! RequestProduct("Cocaine")
        expectMsg(VendingError("MachineNotInitialized"))
      }
      "Report a product not available" in {
        val vendingMachine = system.actorOf(Props[VendingMachineFS])
        vendingMachine ! Initialize(Map("cocaine" -> 0), Map("cocaine" -> 1))
        vendingMachine ! RequestProduct("crack")
        expectMsg(VendingError("ProductNotAvailable"))
      }
      "If i do no inser money" in {
        val vendingMachine = system.actorOf(Props[VendingMachineFS])
        vendingMachine ! Initialize(Map("cocaine" -> 1), Map("cocaine" -> 1))
        vendingMachine ! RequestProduct("cocaine")
        expectMsg(Instruction("Please insert 1"))
        within(1.5 seconds) {
          expectMsg(VendingError("Timeout"))
        }
      }
      "Handle reception of partial Money" in {
        val vendingMachine = system.actorOf(Props[VendingMachineFS])
        vendingMachine ! Initialize(Map("cocaine" -> 1), Map("cocaine" -> 3))
        vendingMachine ! RequestProduct("cocaine")
        expectMsg(Instruction("Please insert 3"))
        vendingMachine ! ReceiveMoney(1)
        expectMsg(Instruction("Please Insert 2"))
        within(2 seconds) {
          expectMsg(VendingError("Timeout"))
          expectMsg(GiveBackChange(1))
        }
      }
      "Deliver the product if i inserts all the money" in {
        val vendingMachine = system.actorOf(Props[VendingMachineFS])
        vendingMachine ! Initialize(Map("cocaine" -> 1), Map("cocaine" -> 3))
        vendingMachine ! RequestProduct("cocaine")
        expectMsg(Instruction("Please insert 3"))
        vendingMachine ! ReceiveMoney(3)
        expectMsg(Deliver("cocaine"))
      }
      "GiveBack change and be able to request money for a new product" in {
        val vendingMachine = system.actorOf(Props[VendingMachineFS])
        vendingMachine ! Initialize(Map("cocaine" -> 1), Map("cocaine" -> 3))
        vendingMachine ! RequestProduct("cocaine")
        expectMsg(Instruction("Please insert 3"))
        vendingMachine ! ReceiveMoney(4)
        expectMsg(Deliver("cocaine"))
        expectMsg(GiveBackChange(1))

      }
    }

  }
}

object  FSMSpec {


  case class Initialize(inventory: Map[String, Int], prices: Map[String, Int])

  case class RequestProduct(product: String)

  case class Instruction(instruction: String)

  case class ReceiveMoney(amount: Int)

  case class Deliver(product: String)

  case class GiveBackChange(amount: Int)

  case class VendingError(reason: String)

  case object ReceiveMoneyTimeout

  class VendingMachine extends Actor with ActorLogging {
    implicit val executionContext: ExecutionContext = context.dispatcher

    override def receive: Receive = idle

    def idle: Receive = {
      case Initialize(inventory, prices) => context.become(operational(inventory, prices))
      case _ => sender() ! VendingError("MachineNotInitialized")
    }

    def operational(inventory: Map[String, Int], prices: Map[String, Int]): Receive = {
      case RequestProduct(product) =>
        inventory.get(product) match {
          case None | Some(0) => sender() ! VendingError("ProductNotAvailable")
          case Some(_) =>
            val price = prices(product)
            sender() ! Instruction(s"Please insert $price")

            context.become(waitForMoney(inventory, product, prices, 0, startReceiveMoneySchedule, sender()))
        }
    }

    def waitForMoney(inventory: Map[String, Int],
                     product: String,
                     prices: Map[String, Int],
                     money: Int,
                     moneyTimeoutSchedule: Cancellable,
                     requester: ActorRef): Receive = {
      case ReceiveMoneyTimeout =>
        requester ! VendingError("Timeout")
        if (money > 0) requester ! GiveBackChange(money)
        context.become(operational(inventory, prices))
      case ReceiveMoney(amount) =>
        moneyTimeoutSchedule.cancel()
        val price = prices(product)
        if (money + amount >= price) {
          requester ! Deliver(product)
          if (money + amount - price > 0) requester ! GiveBackChange(money + amount - price)
          val newStock = inventory(product) - 1
          val newInventory = inventory + (product -> newStock)
          context.become(operational(newInventory, prices))
        } else {
          val remainimgMoney = price - money - amount
          sender() ! Instruction(s"Please Insert $remainimgMoney")
          context.become(waitForMoney(inventory, product, prices, money + amount, startReceiveMoneySchedule, requester))
        }

    }

    def startReceiveMoneySchedule = context.system.scheduler.scheduleOnce(1 seconds) {
      self ! ReceiveMoneyTimeout
    }
  }

  trait VendingState

  case object Idle extends VendingState

  case object Operational extends VendingState

  case object WaitForMoney extends VendingState

  trait VendingData

  case object UnInitialized extends VendingData

  case class Initialized(inventory: Map[String, Int], prices: Map[String, Int]) extends VendingData

  case class WaitForMoneyData(inventory: Map[String, Int], product: String, prices: Map[String, Int], money: Int, requester: ActorRef) extends VendingData

  class VendingMachineFS extends FSM[VendingState, VendingData] {

    startWith(Idle, UnInitialized)
    when(Idle) {
      case Event(Initialize(inventory, prices), UnInitialized) =>
        goto(Operational) using Initialized(inventory, prices)
      case _ =>
        sender() ! VendingError("MachineNotInitialized")
        stay()
    }

    when(Operational) {
      case Event(RequestProduct(product), Initialized(inventory, prices)) => {
        inventory.get(product) match {
          case None | Some(0) => sender() ! VendingError("ProductNotAvailable")
            stay()
          case Some(_) =>
            val price = prices(product)
            sender() ! Instruction(s"Please insert $price")
            goto(WaitForMoney) using WaitForMoneyData(inventory, product, prices, 0, sender())
        }
      }
    }
    when(WaitForMoney,stateTimeout = 1 second) {
      case Event(StateTimeout,WaitForMoneyData(inventory, product, prices, money, requester))=>
        requester ! VendingError("Timeout")
        if (money > 0) requester ! GiveBackChange(money)
        goto(Operational) using  Initialized(inventory,prices)
      case Event(ReceiveMoney(amount), WaitForMoneyData(inventory, product, prices, money, requester)) =>
        val price = prices(product)
        if (money + amount >= price) {
          requester ! Deliver(product)
          if (money + amount - price > 0) requester ! GiveBackChange(money + amount - price)
          val newStock = inventory(product) - 1
          val newInventory = inventory + (product -> newStock)
          goto(Operational) using Initialized(newInventory, prices)
        } else {
          val remainimgMoney = price - money - amount
          sender() ! Instruction(s"Please Insert $remainimgMoney")
          stay() using WaitForMoneyData(inventory, product, prices, money + amount, requester)
        }
    }
    whenUnhandled {
      case Event(_, _) =>
        sender() ! VendingError("CommandNotFound")
        stay()
    }
    onTransition {
      case stateA -> stateB => log.info(s"Transitioning from $stateA to $stateB")
    }

    initialize()

  }

}

