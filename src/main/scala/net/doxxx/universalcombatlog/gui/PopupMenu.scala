package net.doxxx.universalcombatlog.gui

import javax.swing.JPopupMenu
import scala.swing.{SequentialContainer, Component}

class PopupMenu(title0: String) extends Component with SequentialContainer.Wrapper { self: PopupMenu =>
  override lazy val peer: JPopupMenu = new JPopupMenu(title0)

  def show(invoker: Component, x: Int, y: Int) {
    peer.show(invoker.peer, x, y)
  }
}
