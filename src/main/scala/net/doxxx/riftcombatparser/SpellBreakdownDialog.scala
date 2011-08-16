package net.doxxx.riftcombatparser

import swing.{Window, Dialog}

class SpellBreakdownDialog(owner: Window) extends Dialog(owner) {
  val panel = new SpellBreakdownPanel
  contents = panel

  var currentBreakdownType: BreakdownType.Value = BreakdownType.None
  var currentEvents: List[LogEvent] = Nil

  def update(actor: Actor, breakdownType: BreakdownType.Value, events: List[LogEvent]) {
    currentBreakdownType = breakdownType
    currentEvents = events
    title = "%s - %s".format(actor.name, BreakdownType.nameFor(breakdownType))
    panel.update(EventProcessor.breakdown(breakdownType, actor, events))
  }

  def update(actor: Actor) {
    update(actor, currentBreakdownType, currentEvents)
  }
}
