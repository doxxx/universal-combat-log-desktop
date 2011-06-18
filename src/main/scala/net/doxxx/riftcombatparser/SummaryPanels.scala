package net.doxxx.riftcombatparser

import swing._
import swing.TabbedPane.Page

class SummaryPanels extends TabbedPane {

  val panels = Seq(new OverviewSummaryPanel)

  panels foreach { p =>
    pages += new Page(p.title, p)
    listenTo(p)
  }

  deafTo(this) // otherwise publishing SelectedActorChanged causes StackOverflowError

  reactions += {
    case e: SelectedActorChanged => publish(e)
  }

  def current: SummaryPanel = panels(selection.page.index)

}
