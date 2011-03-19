package net.doxxx.riftcombatparser

import swing._
import collection.JavaConversions._
import collection.immutable.TreeSet
import javax.swing.table.{TableRowSorter, AbstractTableModel}
import java.util.Comparator
import javax.swing.{SortOrder, RowSorter}

class SummaryPanel(events: List[Event]) extends ScrollPane {
  contents = new Table {
    model = new DamageSummaryModel(events)
    val rowSorter = new TableRowSorter(model)
    rowSorter.setComparator(1, new IntComparator)
    rowSorter.setSortKeys(List(new RowSorter.SortKey(1, SortOrder.DESCENDING)))
    peer.setRowSorter(rowSorter)
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

class IntComparator extends Comparator[Int] {
  def compare(o1: Int, o2: Int) = o1 - o2
}
