package Testing

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import akka.testkit.{EventFilter, ImplicitSender, TestKit}
import com.typesafe.config.ConfigFactory
import org.scalatest.BeforeAndAfterAll
import org.scalatest.wordspec.AnyWordSpecLike

class InterceptingLogSpec extends TestKit(ActorSystem("interceptingLogSpec",ConfigFactory.load().getConfig("interceptingLogMessages")))
with ImplicitSender
with AnyWordSpecLike
with BeforeAndAfterAll{

  override def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
  }

  import InterceptingLogsSpec._
  "A checkout flow" should {
    "correctly log the dispatch of an order" in {
      val item = "cool Item"
      val cc = "12-123-123123-123"
      EventFilter.info(pattern = s"Order [0-9]+ for item $item has been dispatched",occurrences = 1) intercept  {
        val checkout = system.actorOf(Props[CheckoutActor])
        checkout ! Checkout(item, cc)
      }
    }
  }
}
  object InterceptingLogsSpec {

    case class Checkout(item : String, creditCard: String)
    case class AuthorizeCard(creditCard:String)
    case object PaymentAccepted
    case object PaymentDenied
    case class DispatchOrder(item: String)
    case object Dispatch
    case object OrderConfirmed
    class CheckoutActor extends Actor {
      private val paymentManager = context.actorOf(Props [PaymentManager])
      private val fulfillmentManager = context.actorOf(Props [FulfillmentManager])
      override def receive: Receive = awaitingCheckout

      def awaitingCheckout : Receive = {
        case Checkout(item,creditCard)=>{
          paymentManager ! AuthorizeCard(creditCard)
          context.become(pendingPayment(item))
        }
      }
      def pendingPayment(item: String): Receive = {
        case PaymentAccepted =>
          fulfillmentManager ! DispatchOrder(item)
          context.become(pendingFulfillment(item))
        case PaymentDenied =>
      }
      def pendingFulfillment(item: String) : Receive = {
        case OrderConfirmed => context.become(awaitingCheckout)
      }
    }

    class PaymentManager extends Actor {
      override def receive: Receive = {
        case AuthorizeCard(creditCard)=>
          if(creditCard.startsWith("0")) sender() ! PaymentDenied
          else sender() ! PaymentAccepted
      }
    }

    class FulfillmentManager extends Actor with ActorLogging {
      var orderId = 0
      override def receive: Receive = {
        case DispatchOrder(item: String)=>
          orderId +=1
          log.info(s"Order $orderId for item $item has been dispatched")
          sender() ! OrderConfirmed
      }
    }

  }

