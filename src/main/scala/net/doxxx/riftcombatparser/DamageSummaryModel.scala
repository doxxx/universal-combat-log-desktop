package net.doxxx.riftcombatparser

import javax.swing.table.AbstractTableModel

class DamageSummaryModel extends AbstractTableModel {
  private val ColumnNames = Array("Name", "Damage In", "Damage Out", "Healing In", "Healing Out", "Deaths")

  private var events: List[LogEvent] = Nil
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
      case 5 => data(name).deaths.asInstanceOf[AnyRef]
      case _ => null
    }
  }

  def getColumnCount = ColumnNames.size

  def getRowCount = data.size

  def update(events: List[LogEvent]) {
    summary = EventProcessor.summary(events)
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
}




