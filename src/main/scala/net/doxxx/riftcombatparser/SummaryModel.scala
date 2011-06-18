package net.doxxx.riftcombatparser

import javax.swing.table.AbstractTableModel

class SummaryModel extends AbstractTableModel {
  private val ColumnNames = Array("Name", "Damage In", "DPS In", "Damage Out", "DPS Out", "Healing In", "HPS In", "Healing Out", "HPS Out", "Overhealing", "Deaths")

  private var summary: Map[String, Summary] = Map.empty
  private var filteredSummary: Option[Map[String, Summary]] = None

  def data = filteredSummary getOrElse summary
  def names = data.keySet.toArray

  override def getColumnName(column: Int) = ColumnNames(column)

  def getValueAt(rowIndex: Int, columnIndex: Int): Object = {
    val name = names(rowIndex)
    columnIndex match {
      case 0 => name
      case 1 => data(name).damageIn.asInstanceOf[AnyRef]
      case 2 => data(name).dpsIn.asInstanceOf[AnyRef]
      case 3 => data(name).damageOut.asInstanceOf[AnyRef]
      case 4 => data(name).dpsOut.asInstanceOf[AnyRef]
      case 5 => data(name).healingIn.asInstanceOf[AnyRef]
      case 6 => data(name).hpsIn.asInstanceOf[AnyRef]
      case 7 => data(name).healingOut.asInstanceOf[AnyRef]
      case 8 => data(name).hpsOut.asInstanceOf[AnyRef]
      case 9 => data(name).overhealing.asInstanceOf[AnyRef]
      case 10 => data(name).deaths.asInstanceOf[AnyRef]
      case _ => null
    }
  }

  def getColumnCount = ColumnNames.size

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




