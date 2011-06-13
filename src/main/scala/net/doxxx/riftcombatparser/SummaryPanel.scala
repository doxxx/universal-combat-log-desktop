package net.doxxx.riftcombatparser

import swing._
import collection.JavaConversions._
import event.{Event, TableRowsSelected}
import javax.swing.table.TableRowSorter
import javax.swing.{SortOrder, RowSorter}

class SummaryPanel extends ScrollPane {
  val summaryModel = new SummaryModel
  val table = new Table {
    model = summaryModel
    val rowSorter = new TableRowSorter(model)
    rowSorter.setComparator(1, IntComparator)
    rowSorter.setComparator(2, IntComparator)
    rowSorter.setComparator(3, IntComparator)
    rowSorter.setComparator(4, IntComparator)
    rowSorter.setComparator(5, IntComparator)
    rowSorter.setComparator(6, IntComparator)
    rowSorter.setComparator(7, IntComparator)
    rowSorter.setComparator(8, IntComparator)
    rowSorter.setComparator(9, IntComparator)
    rowSorter.setSortKeys(List(new RowSorter.SortKey(4, SortOrder.DESCENDING)))
    peer.setRowSorter(rowSorter)
    selection.intervalMode = Table.IntervalMode.Single
  }

  contents = table

  listenTo(table.selection)

  reactions += {
    case TableRowsSelected(source, range, adjusting) => {
      if (!adjusting)
        selectedActor match {
          case Some(actor) => publish(SelectedActorChanged(actor))
          case None =>
        }
    }
  }

  def update(fight: Fight) {
    summaryModel.update(fight)
  }

  def applyActorFilter(actors: Set[String]) {
    summaryModel.applyActorFilter(actors)
  }

  def selectedActor: Option[String] = {
    val row = table.selection.rows.anchorIndex
    if (row >= 0) {
      Some(summaryModel.names(table.viewToModelRow(row)))
    }
    else {
      None
    }
  }

  def selectActor(actor: String) {
    val i = summaryModel.names.indexOf(actor)
    if (i >= 0) {
      table.selection.rows += table.modelToViewRow(i)
    }
  }

  def dpsSummaryForClipboard = summaryModel.dpsSummaryForClipboard
  def hpsSummaryForClipboard = summaryModel.hpsSummaryForClipboard
}

case class SelectedActorChanged(actor: String) extends Event
