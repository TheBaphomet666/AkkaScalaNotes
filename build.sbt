name := "Akka-Essentials-Udemy"

version := "0.1"

scalaVersion := "2.13.1"

val akkaVersion = "2.6.1"

resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor-typed" % akkaVersion,
  "com.typesafe.akka" %% "akka-testkit" % akkaVersion
)

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.3" % Runtime
libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.0"


