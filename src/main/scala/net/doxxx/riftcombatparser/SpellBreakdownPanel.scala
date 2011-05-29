package net.doxxx.riftcombatparser

import collection.JavaConversions._
import swing.{Table, ScrollPane}
import javax.swing.table.TableRowSorter
import javax.swing.{SortOrder, RowSorter}

class SpellBreakdownPanel extends ScrollPane {
  val spellBreakdownModel = new SpellBreakdownModel
  val table = new Table {
    model = spellBreakdownModel
    val rowSorter = new TableRowSorter(model)
    rowSorter.setComparator(1, IntComparator)
    rowSorter.setComparator(2, IntComparator)
    rowSorter.setComparator(3, IntComparator)
    rowSorter.setComparator(4, IntComparator)
    rowSorter.setComparator(5, IntComparator)
    rowSorter.setSortKeys(List(new RowSorter.SortKey(1, SortOrder.DESCENDING)))
    peer.setRowSorter(rowSorter)
  }

  contents = table

  def update(events: List[LogEvent]) {
    spellBreakdownModel.update(EventProcessor.spellBreakdown(events))
  }
}
