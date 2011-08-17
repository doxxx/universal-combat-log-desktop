package net.doxxx.riftcombatparser

import collection.JavaConversions._
import swing.ScrollPane
import javax.swing.table.TableRowSorter
import javax.swing.{SortOrder, RowSorter}

class BreakdownPanel extends ScrollPane {
  val spellBreakdownModel = new BreakdownModel
  val table = new FixedTable {
    model = spellBreakdownModel
    val rowSorter = new TableRowSorter(model)
    rowSorter.setComparator(1, IntComparator)
    rowSorter.setComparator(2, IntComparator)
    rowSorter.setComparator(3, IntComparator)
    rowSorter.setComparator(4, IntComparator)
    rowSorter.setComparator(5, IntComparator)
    rowSorter.setSortKeys(List(new RowSorter.SortKey(1, SortOrder.DESCENDING)))
    peer.setRowSorter(rowSorter)
    peer.getColumnModel.getColumn(0).setPreferredWidth(200)
  }

  contents = table

  def update(breakdown: Map[String, Breakdown]) {
    spellBreakdownModel.update(breakdown)
  }
}
