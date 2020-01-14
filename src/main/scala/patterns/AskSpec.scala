package patterns

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.wordspec.AnyWordSpecLike
import akka.pattern.ask
import akka.util.Timeout
import scala.language.postfixOps
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.util.{Failure, Success}
import akka.pattern.pipe
class AskSpec extends TestKit(ActorSystem("askSpec"))
with ImplicitSender
with AnyWordSpecLike
with BeforeAndAfterAll {
  override def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
  }
  import AskSpec._

  "An Authenticator " should {
    import AuthManager._
    "fail to auth a unregistered user" in {
      val authManager = system.actorOf(Props[AuthManager])
      authManager ! Authenticate("jhon","117")
      expectMsg(AuthFailure(AUTH_FAILED_NOT_FOUND))
    }
    "fail to auth if invalid password" in {
      val authManager = system.actorOf(Props[AuthManager])
      authManager ! RegisterUser("jhon", "118")
      authManager ! Authenticate("jhon","117")
      expectMsg(AuthFailure(AUTH_FAILURE_PASSWORD_INCORRECT))
    }

  }
}

object AskSpec {

  case class Read(key:String)
  case class Write(key:String,value: String)
  class KVActor extends Actor with ActorLogging {
    override def receive: Receive = online(Map())

    def online (kv: Map[String,String]): Receive = {
      case Read(key)=>
        log.info(s"Reading value at the key $key")
        sender() ! kv.get(key)
      case Write(key,value)=>
        log.info(s"Writing the value $value fot the key $key")
        context.become(online(kv + (key->value)))
    }
  }

  case class RegisterUser(username:String,password:String)
  case class Authenticate(username:String,password:String)
  case class AuthFailure(message:String)
  case object AuthSuccess
  object AuthManager {
    val AUTH_FAILED_NOT_FOUND = "Username not found"
    val AUTH_FAILURE_PASSWORD_INCORRECT = "Password incorrect"
    val AUTH_FAILURE_SYSTEM= "System error"
  }
  class AuthManager extends Actor with ActorLogging {

    import AuthManager._

    implicit val timeput: Timeout = Timeout(1 second)
    implicit val executionContext: ExecutionContext = context.dispatcher
    protected val authDb = context.actorOf(Props[KVActor])

    override def receive: Receive = {
      case RegisterUser(username, password) =>
        authDb ! Write(username, password)
      case Authenticate(username, password) => handleAuthentication(username, password)

    }

    def handleAuthentication(username: String, password: String) = {
      val originalSender = sender()
      val future = authDb ? Read(username)
      future.onComplete {
        case Success(None) => originalSender ! AuthFailure(AUTH_FAILED_NOT_FOUND)
        case Success(Some(dbPassword)) =>
          if (dbPassword == password) originalSender ! AuthSuccess
          else originalSender ! AuthFailure(AUTH_FAILURE_PASSWORD_INCORRECT)
        case Failure(exception) => originalSender ! AuthFailure(AUTH_FAILURE_SYSTEM)
      }
    }
  }

  class PipedAuthManager extends AuthManager {
    import AuthManager._
    override def handleAuthentication(username: String, password: String): Unit = {
      val future = authDb ? Read(username)
      val passwordFuture = future.mapTo[Option[String]]
      val responseFuture = passwordFuture.map {
        case None => AuthFailure(AUTH_FAILED_NOT_FOUND)
        case Some(dbPassword)=>
          if(dbPassword == password) AuthSuccess
          else AuthFailure(AUTH_FAILURE_PASSWORD_INCORRECT)
      }
      responseFuture.pipeTo(sender())
    }
  }
}
