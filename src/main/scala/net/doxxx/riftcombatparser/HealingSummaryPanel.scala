package net.doxxx.riftcombatparser

import swing._
import event.TableRowsSelected

class HealingSummaryPanel extends ScrollPane with SummaryPanel {
  val summaryModel = new SummaryModel(Seq(SummaryColumns.Name, SummaryColumns.HPSOut, SummaryColumns.HealingOut,
    SummaryColumns.HPSIn, SummaryColumns.HealingIn, SummaryColumns.Overhealing))
  val table = new Table {
    model = summaryModel
    peer.setRowSorter(summaryModel.rowSorter(SummaryColumns.HPSOut))
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

  def title = "Healing"

  def updateImpl(summary: Map[String, Summary]) {
    summaryModel.update(summary)
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
}
