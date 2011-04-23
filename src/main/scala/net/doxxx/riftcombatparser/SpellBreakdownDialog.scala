package net.doxxx.riftcombatparser

import swing.{Window, Dialog}

class SpellBreakdownDialog(owner: Window) extends Dialog(owner) {
  val panel = new SpellBreakdownPanel
  contents = panel

  def update(events: List[LogEvent]) {
    panel.update(events)
  }
}
