import sbt._

class RiftCombatParser(info: ProjectInfo) extends DefaultProject(info) {
  val scalaSwing = "org.scala-lang" % "scala-swing" % "2.8.1"
  override def mainClass = Some("net.doxxx.riftcombatparser.GUIMain")
}
