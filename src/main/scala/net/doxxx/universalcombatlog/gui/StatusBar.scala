package net.doxxx.universalcombatlog.gui

import swing._

class StatusBar extends BoxPanel(Orientation.Horizontal) {
  val message = new Label
  val progress = new ProgressBar

  contents += message
  contents += Swing.HStrut(5)
  contents += progress

  def update(message: String) {
    this.message.text = message
  }
}
