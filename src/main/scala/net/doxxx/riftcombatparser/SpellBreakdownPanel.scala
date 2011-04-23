package net.doxxx.riftcombatparser

import swing.{Table, ScrollPane}

class SpellBreakdownPanel extends ScrollPane {
  val spellBreakdownModel = new SpellBreakdownModel
  val table = new Table {
    model = spellBreakdownModel
  }

  contents = table

  def update(events: List[LogEvent]) {
    spellBreakdownModel.update(EventProcessor.spellBreakdown(events))
  }
}
