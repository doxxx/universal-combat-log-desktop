package net.doxxx.riftcombatparser

import swing._
import javax.swing.table.AbstractTableModel
import collection.immutable.TreeSet

class SummaryPanel(events: List[Event]) extends ScrollPane {
  contents = new Table {
     model = new DamageSummaryModel(events)
  }
}

class DamageSummaryModel(events: List[Event]) extends AbstractTableModel {
  private val data = EventProcessor.damageSummary(events)
  private val sortedNames = (TreeSet.empty[String] ++ data.keySet).toArray

  override def getColumnName(column: Int) = column match {
    case 0 => "Name"
    case 1 => "Total Damage"
  }

  def getValueAt(rowIndex: Int, columnIndex: Int): Object = {
    columnIndex match {
      case 0 => sortedNames(rowIndex)
      case 1 => data(sortedNames(rowIndex)).asInstanceOf[AnyRef]
      case _ => null
    }
  }

  def getColumnCount = 2

  def getRowCount = data.size
}
