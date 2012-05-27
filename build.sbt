organization := "net.doxxx"

name := "RiftCombatParser"

normalizedName := "rift-combat-parser"

version := "1.0"

scalaVersion := "2.9.1"

libraryDependencies += "org.scala-lang" % "scala-swing" % "2.9.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.7.2" % "test"

mainClass in (Compile, run) := Some("net.doxxx.riftcombatparser.GUIMain")
