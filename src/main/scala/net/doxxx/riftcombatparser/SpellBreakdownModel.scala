package net.doxxx.riftcombatparser

import javax.swing.table.AbstractTableModel

class SpellBreakdownModel extends AbstractTableModel {
  private val ColumnNames = Array("Name", "Damage", "Healing", "Count", "Miss %", "Crit %")

  private var breakdown: Map[String, SpellBreakdown] = Map.empty

  def data = breakdown

  override def getColumnName(column: Int) = ColumnNames(column)

  def getValueAt(rowIndex: Int, columnIndex: Int): Object = {
    val names = data.keySet.toArray
    val name = names(rowIndex)
    val spellData = data(name)
    val count = spellData.hits + spellData.misses + spellData.crits
    val value = columnIndex match {
      case 0 => name
      case 1 => spellData.damage
      case 2 => spellData.healing
      case 3 => count
      case 4 => percent(spellData.misses, count)
      case 5 => percent(spellData.crits, count)
      case _ => null
    }
    value.asInstanceOf[AnyRef]
  }

  def getColumnCount = ColumnNames.size

  def getRowCount = data.size

  def update(breakdown: Map[String, SpellBreakdown]) {
    this.breakdown = breakdown
    fireTableDataChanged()
  }

  def percent(num: Int, denom: Int): Double = {
    (num:Double) / denom * 100
  }

}
