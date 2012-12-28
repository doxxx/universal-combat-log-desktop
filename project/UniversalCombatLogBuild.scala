import sbt._
import Keys._

object UniversalCombatLogBuild extends Build {
  lazy val projectScalaVersion = "2.9.2"
  lazy val guiMainClass = Some("net.doxxx.universalcombatlog.GUIMain")

  lazy val root = {
    Project("universal-combat-log", file("."),
      settings = Defaults.defaultSettings ++ Seq(
        organization := "net.doxxx",
        name := "UniversalCombatLog",
        normalizedName := "universal-combat-log",
        version := "1.0",
        scalaVersion := projectScalaVersion,
        libraryDependencies := Seq(
          "org.scala-lang" % "scala-swing" % projectScalaVersion,
          "org.scalatest" %% "scalatest" % "1.7.2" % "test",
          "com.github.scala-incubator.io" %% "scala-io-core" % "0.4.1-seq",
          "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.1-seq"
        ),
        mainClass in(Compile, packageBin) := guiMainClass,
        mainClass in(Compile, run) := guiMainClass
      )
    )
  }
}
