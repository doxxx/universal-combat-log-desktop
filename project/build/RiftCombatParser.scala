import sbt._

class RiftCombatParser(info: ProjectInfo) extends DefaultProject(info) {
  val scalaToolsSnapshots = "Scala-Tools Maven2 Snapshots Repository" at "http://scala-tools.org/repo-snapshots"
  
//  val scalaSwing = "org.scala-lang" % "scala-swing" % "2.8.1"
  val scalaSwing = "EPFL" %% "scala-swing" % "1.0" withSources() withJavadoc()
  val scalaTest = "org.scalatest" % "scalatest" % "1.4-SNAPSHOT" withSources() withJavadoc()
  override def mainClass = Some("net.doxxx.riftcombatparser.GUIMain")
  //override def compileOptions = super.compileOptions ++ List(Unchecked)
}
