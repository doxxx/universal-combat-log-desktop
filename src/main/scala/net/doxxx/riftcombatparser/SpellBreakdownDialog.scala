package net.doxxx.riftcombatparser

import swing.{Window, Dialog}

class SpellBreakdownDialog(owner: Window) extends Dialog(owner) {
  val panel = new SpellBreakdownPanel
  contents = panel

  def update(actor:String, events: List[LogEvent]) {
    title = actor
    panel.update(events)
  }
}
