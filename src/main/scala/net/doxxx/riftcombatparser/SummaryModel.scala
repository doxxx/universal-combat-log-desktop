package net.doxxx.riftcombatparser

import collection.JavaConversions._
import javax.swing.table.{TableRowSorter, AbstractTableModel}
import javax.swing.{SortOrder, RowSorter}

object SummaryColumns extends Enumeration {
  type Column = Value
  val Name, DamageIn, DPSIn, DamageOut, DPSOut, HealingIn, HPSIn, HealingOut, HPSOut, Overhealing, Deaths = Value
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
    Deaths -> "Deaths"
  )
}

class SummaryModel(columns: Seq[SummaryColumns.Column]) extends AbstractTableModel {
  import SummaryColumns._

  private var summary: Map[String, Summary] = Map.empty
  private var filteredSummary: Option[Map[String, Summary]] = None
  private def data = filteredSummary getOrElse summary

  def names = data.keySet.toArray

  def indexOfColumn(column: Column): Int = columns.indexOf(column)

  def rowSorter(default: Column): TableRowSorter[SummaryModel] = {
    val rowSorter = new TableRowSorter(this)
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
    val name = names(rowIndex)
    columns(columnIndex) match {
      case Name => name
      case DamageIn => data(name).damageIn.asInstanceOf[AnyRef]
      case DPSIn => data(name).dpsIn.asInstanceOf[AnyRef]
      case DamageOut => data(name).damageOut.asInstanceOf[AnyRef]
      case DPSOut => data(name).dpsOut.asInstanceOf[AnyRef]
      case HealingIn => data(name).healingIn.asInstanceOf[AnyRef]
      case HPSIn => data(name).hpsIn.asInstanceOf[AnyRef]
      case HealingOut => data(name).healingOut.asInstanceOf[AnyRef]
      case HPSOut => data(name).hpsOut.asInstanceOf[AnyRef]
      case Overhealing => data(name).overhealing.asInstanceOf[AnyRef]
      case Deaths => data(name).deaths.asInstanceOf[AnyRef]
      case _ => null
    }
  }

  def getColumnCount = columns.size

  def getRowCount = data.size

  def update(fight: Fight) {
    summary = EventProcessor.summary(fight)
    fireTableDataChanged()
  }

  def applyActorFilter(actors: Set[String]) {
    filteredSummary =
      if (actors.isEmpty)
        None
      else
        Some(summary filter { case (actor, sum) => actors.contains(actor) })
    fireTableDataChanged()
  }

  def dpsSummary = data.map {case (name, summary) => name -> summary.dpsOut}
  def dpsSorted = dpsSummary.toList.sortBy {case (name, value) => value}.reverse
  def hpsSummary = data.map {case (name, summary) => name -> summary.hpsOut}
  def hpsSorted = hpsSummary.toList.sortBy {case (name, value) => value}.reverse
  def raidDPS = dpsSummary.map{case (name, value) => value}.sum
  def raidHPS = hpsSummary.map{case (name, value) => value}.sum

  def dpsSummaryForClipboard:String = {
    val dps = dpsSorted.take(10).filter { case (name, value) => value > 0 }
    "DPS: Raid:%d - %s".format(
      raidDPS,
      (dps.map {case (name, value) => "%.4s:%d".format(name, value)}).mkString(", ")
    )
  }

  def hpsSummaryForClipboard:String = {
    val hps = hpsSorted.take(10).filter { case (name, value) => value > 0 }
    "HPS: Raid:%d - %s".format(
      raidHPS,
      (hps.map {case (name, value) => "%.4s:%d".format(name, value)}).mkString(", ")
    )
  }
}




