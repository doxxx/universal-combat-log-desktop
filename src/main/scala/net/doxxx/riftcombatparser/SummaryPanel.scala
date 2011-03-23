package net.doxxx.riftcombatparser

import swing._
import collection.JavaConversions._
import javax.swing.table.{TableRowSorter, AbstractTableModel}
import java.util.Comparator
import javax.swing.{SortOrder, RowSorter}

class SummaryPanel extends ScrollPane {
  val summaryModel = new DamageSummaryModel

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

  def updateEvents(summary: Map[String, Summary]) {
    summaryModel.update(summary)
  }

  def applyActorFilter(actors: List[String]) {
    summaryModel.applyActorFilter(actors)
  }
}

class DamageSummaryModel extends AbstractTableModel {
  private val ColumnNames = Array("Name", "Damage In", "Damage Out", "Healing In", "Healing Out")

  private var summary: Map[String, Summary] = Map.empty
  private var filteredSummary: Option[Map[String, Summary]] = None

  def data = filteredSummary getOrElse summary

  override def getColumnName(column: Int) = ColumnNames(column)

  def getValueAt(rowIndex: Int, columnIndex: Int): Object = {
    val names = data.keySet.toArray
    val name = names(rowIndex)
    columnIndex match {
      case 0 => name
      case 1 => data(name).damageIn.asInstanceOf[AnyRef]
      case 2 => data(name).damageOut.asInstanceOf[AnyRef]
      case 3 => data(name).healingIn.asInstanceOf[AnyRef]
      case 4 => data(name).healingOut.asInstanceOf[AnyRef]
      case _ => null
    }
  }

  def getColumnCount = ColumnNames.size

  def getRowCount = data.size

  def update(summary: Map[String, Summary]) {
    this.summary = summary
    fireTableDataChanged()
  }

  def applyActorFilter(actors: List[String]) {
    filteredSummary =
      if (actors.isEmpty)
        None
      else
        Some(summary filter { case (actor, sum) => actors.contains(actor) })
    fireTableDataChanged()
  }
}

class IntComparator extends Comparator[Int] {
  def compare(o1: Int, o2: Int) = o1 - o2
}
