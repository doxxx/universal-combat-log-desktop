package net.doxxx.universalcombatlog.gui

import java.awt.event.{MouseEvent, MouseAdapter}
import javax.swing.JTable
import net.doxxx.universalcombatlog.gui.SummaryColumns._
import net.doxxx.universalcombatlog.parser.Entity
import net.doxxx.universalcombatlog.{Summary, BreakdownType}
import scala.swing._
import scala.swing.event.{ButtonClicked, Event, TableRowsSelected}

class SummaryPanel(val title: String, columns: Seq[Column], defaultColumn: Column) extends ScrollPane {
  val MI_BreakdownOutgoingDamageBySpell = new MenuItem("By Spell")
  val MI_BreakdownOutgoingDamageByTarget = new MenuItem("By Target")
  val MI_BreakdownOutgoingHealingBySpell = new MenuItem("By Spell")
  val MI_BreakdownOutgoingHealingByTarget = new MenuItem("By Target")
  val MI_BreakdownIncomingDamageBySpell = new MenuItem("By Spell")
  val MI_BreakdownIncomingDamageByActor = new MenuItem("By Actor")
  val MI_BreakdownIncomingHealingBySpell = new MenuItem("By Spell")
  val MI_BreakdownIncomingHealingByActor = new MenuItem("By Actor")
  val MI_DeathLog = new MenuItem("Death Log")
  val popupMenu = new PopupMenu("Breakdown") {
    contents += new Menu("Outgoing") {
      contents += new Menu("Damage") {
        contents += MI_BreakdownOutgoingDamageBySpell
        contents += MI_BreakdownOutgoingDamageByTarget
      }
      contents += new Menu("Healing") {
        contents += MI_BreakdownOutgoingHealingBySpell
        contents += MI_BreakdownOutgoingHealingByTarget
      }
    }
    contents += new Menu("Incoming") {
      contents += new Menu("Damage") {
        contents += MI_BreakdownIncomingDamageBySpell
        contents += MI_BreakdownIncomingDamageByActor
      }
      contents += new Menu("Healing") {
        contents += MI_BreakdownIncomingHealingBySpell
        contents += MI_BreakdownIncomingHealingByActor
      }
    }
    contents += MI_DeathLog
  }

  val summaryModel = new SummaryModel(columns)
  val table: FixedTable = new FixedTable {
    model = summaryModel
    peer.setRowSorter(summaryModel.rowSorter(defaultColumn))
    selection.intervalMode = Table.IntervalMode.Single
    peer.getColumnModel.getColumn(0).setPreferredWidth(200)

    peer.addMouseListener(new MouseAdapter {
      override def mousePressed(e: MouseEvent) {
        evaluatePopup(e)
      }

      override def mouseReleased(e: MouseEvent) {
        evaluatePopup(e)
      }

      private def evaluatePopup(e: MouseEvent) {
        if (e.isPopupTrigger) {
          val source = e.getSource.asInstanceOf[JTable]
          val row = source.rowAtPoint(e.getPoint)
          val column = source.columnAtPoint(e.getPoint)

          if (!source.isRowSelected(row))
            source.changeSelection(row, column, false, false)

          popupMenu.show(table, e.getX, e.getY)
        }
      }
    })
  }

  contents = table

  listenTo(table.selection)
  listenTo(MI_BreakdownOutgoingDamageBySpell)
  listenTo(MI_BreakdownOutgoingDamageByTarget)
  listenTo(MI_BreakdownOutgoingHealingBySpell)
  listenTo(MI_BreakdownOutgoingHealingByTarget)
  listenTo(MI_BreakdownIncomingDamageBySpell)
  listenTo(MI_BreakdownIncomingDamageByActor)
  listenTo(MI_BreakdownIncomingHealingBySpell)
  listenTo(MI_BreakdownIncomingHealingByActor)
  listenTo(MI_DeathLog)

  reactions += {
    case TableRowsSelected(source, range, adjusting) => {
      if (!adjusting)
        selectedActor match {
          case Some(actor) => publish(SelectedActorChanged(actor))
          case None =>
        }
    }
    case ButtonClicked(MI_DeathLog) => publish(DeathLogRequested(selectedActor.get))
    case ButtonClicked(button) => {
      val breakdownType = button match {
        case MI_BreakdownOutgoingDamageBySpell => BreakdownType.OutgoingDamageBySpell
        case MI_BreakdownOutgoingDamageByTarget => BreakdownType.OutgoingDamageByTarget
        case MI_BreakdownOutgoingHealingBySpell => BreakdownType.OutgoingHealingBySpell
        case MI_BreakdownOutgoingHealingByTarget => BreakdownType.OutgoingHealingByTarget
        case MI_BreakdownIncomingDamageBySpell => BreakdownType.IncomingDamageBySpell
        case MI_BreakdownIncomingDamageByActor => BreakdownType.IncomingDamageByActor
        case MI_BreakdownIncomingHealingBySpell => BreakdownType.IncomingHealingBySpell
        case MI_BreakdownIncomingHealingByActor => BreakdownType.IncomingHealingByActor
        case _ => throw new RuntimeException("Unrecognized button")
      }
      publish(BreakdownRequested(selectedActor.get, breakdownType))
    }
  }

  def update(summary: Map[Entity, Summary]) {
    val oldActor = selectedActor
    summaryModel.update(summary)
    if (oldActor.isDefined) selectActor(oldActor.get)
  }

  def selectedActor: Option[Entity] = {
    val row = table.selection.rows.anchorIndex
    if (row >= 0) {
      Some(summaryModel.actors(table.viewToModelRow(row)))
    }
    else {
      None
    }
  }

  def selectActor(actor: Entity) {
    val i = summaryModel.actors.indexOf(actor)
    if (i >= 0) {
      table.selection.rows += table.modelToViewRow(i)
    }
  }
}

case class SelectedActorChanged(actor: Entity) extends Event
case class BreakdownRequested(actor: Entity, breakdownType: BreakdownType.Value) extends Event
case class DeathLogRequested(actor: Entity) extends Event
