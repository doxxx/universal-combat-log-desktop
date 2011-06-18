package net.doxxx.riftcombatparser

import swing._
import event.{Event, TableRowsSelected}

class SummaryPanel extends ScrollPane {
  val summaryModel = new SummaryModel(Seq(SummaryColumns.Name, SummaryColumns.DPSOut, SummaryColumns.HPSOut, SummaryColumns.Deaths))
  val table = new Table {
    model = summaryModel
    peer.setRowSorter(summaryModel.rowSorter(SummaryColumns.DPSOut))
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
