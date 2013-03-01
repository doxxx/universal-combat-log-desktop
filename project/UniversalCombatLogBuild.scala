import sbt._
import Keys._

object UniversalCombatLogBuild extends Build {
  lazy val projectScalaVersion = "2.9.2"
  lazy val guiMainClass = Some("net.doxxx.universalcombatlog.gui.Main")

  lazy val root = {
    Project("universal-combat-log", file("."),
      settings = Defaults.defaultSettings ++ Seq(
        organization := "net.doxxx",
        name := "UniversalCombatLog",
        normalizedName := "universal-combat-log",
        version := "1.0",
        scalaVersion := projectScalaVersion,
        resolvers += "spray" at "http://repo.spray.cc/",
        resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
        libraryDependencies := Seq(
          "org.scala-lang" % "scala-swing" % projectScalaVersion,
          "org.scalatest" %% "scalatest" % "1.7.2" % "test",
          "com.github.scala-incubator.io" %% "scala-io-core" % "0.4.1-seq",
          "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.1-seq",
          "se.scalablesolutions.akka" % "akka-actor" % "1.3.1",
          "org.slf4j" % "slf4j-simple" % "1.7.2",
          "cc.spray" % "spray-can" % "0.9.3",
          "cc.spray" %%  "spray-json" % "1.1.1"
        ),
        mainClass in(Compile, packageBin) := guiMainClass,
        mainClass in(Compile, run) := guiMainClass
      )
    )
  }
}
