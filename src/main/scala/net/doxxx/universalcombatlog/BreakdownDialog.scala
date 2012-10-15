package net.doxxx.universalcombatlog

import parser.{Entity, LogEvent}
import swing.{Window, Dialog}

class BreakdownDialog(owner: Window) extends Dialog(owner) {
  val panel = new BreakdownPanel
  contents = panel

  var currentBreakdownType: BreakdownType.Value = BreakdownType.None
  var currentEvents: List[LogEvent] = Nil

  def update(actor: Entity, breakdownType: BreakdownType.Value, events: List[LogEvent]) {
    currentBreakdownType = breakdownType
    currentEvents = events
    title = "%s - %s".format(actor.name, BreakdownType.nameFor(breakdownType))
    panel.update(EventProcessor.breakdown(breakdownType, actor, events))
  }

  def update(actor: Entity) {
    update(actor, currentBreakdownType, currentEvents)
  }
}
