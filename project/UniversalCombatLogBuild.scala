import sbt._
import Keys._

object UniversalCombatLogBuild extends Build {
  lazy val projectScalaVersion = "2.10.0"
  lazy val guiMainClass = Some("net.doxxx.universalcombatlog.gui.Main")

  lazy val root = {
    Project("universal-combat-log", file("."),
      settings = Defaults.defaultSettings ++ Seq(
        organization := "net.doxxx",
        name := "UniversalCombatLog",
        normalizedName := "universal-combat-log",
        version := "1.0",
        scalaVersion := projectScalaVersion,
        resolvers += "spray" at "http://repo.spray.io/",
        resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
        libraryDependencies := Seq(
          "org.scala-lang" % "scala-swing" % projectScalaVersion,
          "org.scalatest" %% "scalatest" % "1.9.1" % "test",
          "com.github.scala-incubator.io" %% "scala-io-core" % "0.4.2",
          "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.2",
          "com.typesafe.akka" %% "akka-actor" % "2.1.1",
          "org.slf4j" % "slf4j-simple" % "1.7.2",
          "io.spray" % "spray-can" % "1.1-M7",
          "io.spray" %% "spray-json" % "1.2.3"
        ),
        mainClass in(Compile, packageBin) := guiMainClass,
        mainClass in(Compile, run) := guiMainClass
      )
    )
  }
}
