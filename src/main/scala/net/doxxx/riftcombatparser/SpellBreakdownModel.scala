package net.doxxx.riftcombatparser

import javax.swing.table.AbstractTableModel

class SpellBreakdownModel extends AbstractTableModel {
  private val ColumnNames = Array("Name", "Damage", "Healing", "Hits", "Misses", "Crits")

  private var breakdown: Map[String, SpellBreakdown] = Map.empty

  def data = breakdown

  override def getColumnName(column: Int) = ColumnNames(column)

  def getValueAt(rowIndex: Int, columnIndex: Int): Object = {
    val names = data.keySet.toArray
    val name = names(rowIndex)
    columnIndex match {
      case 0 => name
      case 1 => data(name).damage.asInstanceOf[AnyRef]
      case 2 => data(name).healing.asInstanceOf[AnyRef]
      case 3 => data(name).hits.asInstanceOf[AnyRef]
      case 4 => data(name).misses.asInstanceOf[AnyRef]
      case 5 => data(name).crits.asInstanceOf[AnyRef]
      case _ => null
    }
  }

  def getColumnCount = ColumnNames.size

  def getRowCount = data.size

  def update(breakdown: Map[String, SpellBreakdown]) {
    this.breakdown = breakdown
    fireTableDataChanged()
  }

}




