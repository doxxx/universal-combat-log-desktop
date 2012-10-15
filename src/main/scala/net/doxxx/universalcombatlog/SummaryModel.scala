package net.doxxx.universalcombatlog

import collection.JavaConversions._
import javax.swing.table.{TableRowSorter, AbstractTableModel}
import javax.swing.{SortOrder, RowSorter}
import parser.Entity

object SummaryColumns extends Enumeration {
  type Column = Value
  val Name, DamageIn, DPSIn, DamageOut, DPSOut, HealingIn, HPSIn, HealingOut, HPSOut, Overhealing, Deaths, CombatTime = Value
  val ColumnNames = Map(
    Name -> "Name",
    DamageIn -> "Damage In",
    DPSIn -> "DPS In",
    DamageOut -> "Damage Out",
    DPSOut -> "DPS Out",
    HealingIn -> "Healing In",
    HPSIn -> "HPS In",
    HealingOut -> "Healing Out",
    HPSOut -> "HPS Out",
    Overhealing -> "Overhealing",
    Deaths -> "Deaths",
    CombatTime -> "Combat Time"
  )
}

class SummaryModel(columns: Seq[SummaryColumns.Column]) extends AbstractTableModel {
  import SummaryColumns._

  private var data: Map[Entity, Summary] = Map.empty

  def actors = data.keySet.toArray
  def names = actors.map(_.name)

  def indexOfColumn(column: Column): Int = columns.indexOf(column)

  def rowSorter(default: Column): TableRowSorter[SummaryModel] = {
    val rowSorter = new TableRowSorter[SummaryModel](this)
    for (i <- Range(0, columns.size)) {
      columns(i) match {
        case Name => // do nothing
        case _ => rowSorter.setComparator(i, IntComparator)
      }
    }
    rowSorter.setSortKeys(List(new RowSorter.SortKey(indexOfColumn(default), SortOrder.DESCENDING)))
    rowSorter
  }

  override def getColumnName(column: Int) = ColumnNames(columns(column))

  def getValueAt(rowIndex: Int, columnIndex: Int): Object = {
    val actor = actors(rowIndex)
    columns(columnIndex) match {
      case Name => actor.name
      case DamageIn => data(actor).damageIn.asInstanceOf[AnyRef]
      case DPSIn => data(actor).dpsIn.asInstanceOf[AnyRef]
      case DamageOut => data(actor).damageOut.asInstanceOf[AnyRef]
      case DPSOut => data(actor).dpsOut.asInstanceOf[AnyRef]
      case HealingIn => data(actor).healingIn.asInstanceOf[AnyRef]
      case HPSIn => data(actor).hpsIn.asInstanceOf[AnyRef]
      case HealingOut => data(actor).healingOut.asInstanceOf[AnyRef]
      case HPSOut => data(actor).hpsOut.asInstanceOf[AnyRef]
      case Overhealing => data(actor).overhealing.asInstanceOf[AnyRef]
      case Deaths => data(actor).deaths.asInstanceOf[AnyRef]
      case CombatTime => (data(actor).combatTime / 1000).asInstanceOf[AnyRef]
      case _ => null
    }
  }

  def getColumnCount = columns.size

  def getRowCount = data.size

  def update(summary: Map[Entity, Summary]) {
    data = summary
    fireTableDataChanged()
  }

}
