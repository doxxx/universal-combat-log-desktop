package net.doxxx.riftcombatparser

import swing.{Window, Dialog}

class SpellBreakdownDialog(owner: Window) extends Dialog(owner) {
  val panel = new SpellBreakdownPanel
  contents = panel

  def update(actor: Actor, breakdown: Map[String, Breakdown]) {
    title = actor.name
    panel.update(breakdown)
  }
}
