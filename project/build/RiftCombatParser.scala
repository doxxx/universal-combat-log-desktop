import sbt._

class RiftCombatParser(info: ProjectInfo) extends DefaultProject(info) {
//  val scalaSwing = "org.scala-lang" % "scala-swing" % "2.8.1"
  val scalaSwing = "EPFL" %% "scala-swing" % "1.0" withSources()
  override def mainClass = Some("net.doxxx.riftcombatparser.GUIMain")
}
