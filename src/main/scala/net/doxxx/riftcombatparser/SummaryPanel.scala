package net.doxxx.riftcombatparser

import swing._
import event.{Event, TableRowsSelected}
import net.doxxx.riftcombatparser.SummaryColumns._

class SummaryPanel(val title: String, columns: Seq[Column], defaultColumn: Column) extends ScrollPane {
  val summaryModel = new SummaryModel(columns)
  val table = new Table {
    model = summaryModel
    peer.setRowSorter(summaryModel.rowSorter(defaultColumn))
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

  def update(summary: Map[Actor, Summary]) {
    val oldActor = selectedActor
    summaryModel.update(summary)
    if (oldActor.isDefined) selectActor(oldActor.get)
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

case class SelectedActorChanged(actor: String) extends Event
