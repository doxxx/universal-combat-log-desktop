package net.doxxx.riftcombatparser

import swing._
import collection.JavaConversions._
import collection.immutable.TreeSet
import javax.swing.table.{TableRowSorter, AbstractTableModel}
import java.util.Comparator
import javax.swing.{SortOrder, RowSorter}

class SummaryPanel(events: List[Event]) extends ScrollPane {
  val summaryModel = new DamageSummaryModel(events)

  contents = new Table {
    model = summaryModel
    val rowSorter = new TableRowSorter(model)
    rowSorter.setComparator(1, new IntComparator)
    rowSorter.setComparator(2, new IntComparator)
    rowSorter.setComparator(3, new IntComparator)
    rowSorter.setComparator(4, new IntComparator)
    rowSorter.setSortKeys(List(new RowSorter.SortKey(2, SortOrder.DESCENDING)))
    peer.setRowSorter(rowSorter)
  }

  def updateEvents(events: List[Event]) {
    summaryModel.update(events)
  }
}

class DamageSummaryModel(events: List[Event]) extends AbstractTableModel {
  private var data = EventProcessor.summary(events)
  private var names = data.keySet.toArray

  override def getColumnName(column: Int) = column match {
    case 0 => "Name"
    case 1 => "Damage In"
    case 2 => "Damage Out"
    case 3 => "Healing In"
    case 4 => "Healing Out"
  }

  def getValueAt(rowIndex: Int, columnIndex: Int): Object = {
    columnIndex match {
      case 0 => names(rowIndex)
      case 1 => data(names(rowIndex)).damageIn.asInstanceOf[AnyRef]
      case 2 => data(names(rowIndex)).damageOut.asInstanceOf[AnyRef]
      case 3 => data(names(rowIndex)).healingIn.asInstanceOf[AnyRef]
      case 4 => data(names(rowIndex)).healingOut.asInstanceOf[AnyRef]
      case _ => null
    }
  }

  def getColumnCount = 5

  def getRowCount = data.size

  def update(events: List[Event]) {
    data = EventProcessor.summary(events)
    names = data.keySet.toArray
    fireTableDataChanged()
  }
}

class IntComparator extends Comparator[Int] {
  def compare(o1: Int, o2: Int) = o1 - o2
}
