package net.doxxx.universalcombatlog.gui

import scala.swing.Table

/**
* Fixex Table sorting by mapping row index from view to model correctly.
*/
class FixedTable extends Table {
  override def apply(row: Int, column: Int): Any = model.getValueAt(viewToModelRow(row), viewToModelColumn(column))

  def viewToModelRow(idx: Int) = peer.convertRowIndexToModel(idx)
  def modelToViewRow(idx: Int) = peer.convertRowIndexToView(idx)
}
