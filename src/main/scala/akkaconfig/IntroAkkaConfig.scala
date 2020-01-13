package akkaconfig

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import com.typesafe.config.ConfigFactory

object IntroAkkaConfig extends App {

class SimpleLoggingActor extends Actor with ActorLogging {
  override def receive: Receive = {
    case msg => log.info(msg.toString)
  }
}


  val configString =
    """
      | akka {
      | loglevel= "ERROR"
      | }
      |""".stripMargin
  val config = ConfigFactory.parseString(configString)
  val system = ActorSystem("configDemo",ConfigFactory.load(config))
  val actor = system.actorOf(Props[SimpleLoggingActor])
  actor ! "Message lol"

  val configuredSystemFile = ActorSystem("ConfigByFile")
  val configuredActor = configuredSystemFile.actorOf(Props[SimpleLoggingActor])

  configuredActor ! "from file"
  val specialConfig = ConfigFactory.load().getConfig("mySpecialConfig")
  val separatedConfigSystem = ActorSystem("ConfigBySeparateFile",specialConfig)
  val defaultActor = separatedConfigSystem.actorOf(Props[SimpleLoggingActor])

  defaultActor ! "lolaaa"

  val otherConf= ConfigFactory.load("secretFolder/secret.conf")
  println(s"seperate config log level = ${otherConf.getString("akka.loglevel")}")

  val jsonConf= ConfigFactory.load("json/jsonConfig.json")
  println(s" json config : ${jsonConf.getString("aJsonProperty")}")

  val propConf= ConfigFactory.load("props/propsConf.properties")
  println(s" prop config ${propConf.getString("my.simpleProp")}")
}
