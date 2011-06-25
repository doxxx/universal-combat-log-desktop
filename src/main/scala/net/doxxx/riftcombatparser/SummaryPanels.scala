package net.doxxx.riftcombatparser

import swing._
import swing.TabbedPane.Page
import net.doxxx.riftcombatparser.SummaryColumns._

class SummaryPanels extends TabbedPane {

  val panels = Seq(
    new SummaryPanel("Overview", Seq(Name, DPSOut, HPSOut, Deaths), DPSOut),
    new SummaryPanel("Damage", Seq(Name, DPSOut, DamageOut, DPSIn, DamageIn), DPSOut),
    new SummaryPanel("Healing", Seq(Name, HPSOut, HealingOut, HPSIn, HealingIn, Overhealing), HPSOut)
  )

  panels foreach { p =>
    pages += new Page(p.title, p)
    listenTo(p)
  }

  deafTo(this) // otherwise publishing SelectedActorChanged causes StackOverflowError

  reactions += {
    case e: SelectedActorChanged => publish(e)
  }

  def current: SummaryPanel = panels(selection.page.index)
  def update(summary: Map[Actor, Summary]) {
    panels foreach { _.update(summary) }
  }
}
