import sbt._
import Keys._

object RiftCombatParserBuild extends Build {
  lazy val guiMainClass = Some("net.doxxx.riftcombatparser.GUIMain")

  lazy val root = {
    Project("rift-combat-parser", file("."),
      settings = Defaults.defaultSettings ++ Seq(
        organization := "net.doxxx",
        name := "RiftCombatParser",
        normalizedName := "rift-combat-parser",
        version := "1.0",
        scalaVersion := "2.9.1",
        libraryDependencies := Seq(
          "org.scala-lang" % "scala-swing" % "2.9.1",
          "org.scalatest" %% "scalatest" % "1.7.2" % "test"
        ),
        mainClass in(Compile, packageBin) := guiMainClass,
        mainClass in(Compile, run) := guiMainClass
      )
    )
  }
}
