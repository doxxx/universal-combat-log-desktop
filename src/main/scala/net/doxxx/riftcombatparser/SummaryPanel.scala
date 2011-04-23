package net.doxxx.riftcombatparser

import swing._
import collection.JavaConversions._
import javax.swing.table.TableRowSorter
import javax.swing.{SortOrder, RowSorter}

class SummaryPanel extends ScrollPane {
  val summaryModel = new DamageSummaryModel
  val table = new Table {
    model = summaryModel
    val rowSorter = new TableRowSorter(model)
    rowSorter.setComparator(1, IntComparator)
    rowSorter.setComparator(2, IntComparator)
    rowSorter.setComparator(3, IntComparator)
    rowSorter.setComparator(4, IntComparator)
    rowSorter.setComparator(5, IntComparator)
    rowSorter.setSortKeys(List(new RowSorter.SortKey(2, SortOrder.DESCENDING)))
    peer.setRowSorter(rowSorter)
    selection.intervalMode = Table.IntervalMode.Single
  }

  contents = table

  def update(events: List[LogEvent]) {
    summaryModel.update(events)
  }

  def applyActorFilter(actors: Set[String]) {
    summaryModel.applyActorFilter(actors)
  }

  def selectedActor: String = {
    summaryModel.names(table.viewToModelRow(table.selection.rows.anchorIndex))
  }
}
