package net.doxxx.riftcombatparser

import scala.swing._
import io.Source

object GUIMain extends SimpleSwingApplication {
  def top = new MainFrame {
    title = "Rift Combat Parser"

    contents = new SummaryPanel(new Parser(Source.fromFile("CombatLog.txt")).parse())
  }
}
