package net.doxxx.riftcombatparser

import swing._
import event.ButtonClicked
import swing.TabbedPane.Page
import net.doxxx.riftcombatparser.SummaryColumns._
import java.awt.Toolkit
import java.awt.datatransfer.StringSelection

class SummaryPanels extends BoxPanel(Orientation.Vertical) {

  val panels = Seq(
    new SummaryPanel("Overview", Seq(Name, DPSOut, HPSOut, Deaths, CombatTime), DPSOut),
    new SummaryPanel("Damage", Seq(Name, DPSOut, DamageOut, DPSIn, DamageIn), DPSOut),
    new SummaryPanel("Healing", Seq(Name, HPSOut, HealingOut, HPSIn, HealingIn, Overhealing), HPSOut)
  )

  val copyDPSButton = new Button("Copy DPS") {
    enabled = false
  }
  val copyHPSButton = new Button("Copy HPS") {
    enabled = false
  }
  val breakdownButton = new Button("Breakdown") {
    enabled = false
  }
  val spellBreakdownDialog = new SpellBreakdownDialog(GUIMain.top)

  contents += new BoxPanel(Orientation.Horizontal) {
    contents += new Label {
      text = "Summary"
    }
    contents += Swing.HGlue
    contents += copyDPSButton
    contents += Swing.HStrut(5)
    contents += copyHPSButton
    contents += Swing.HStrut(5)
    contents += breakdownButton
    maximumSize = new Dimension(maximumSize.width, preferredSize.height)
  }

  contents += Swing.VStrut(5)

  val tabs = new TabbedPane {
    panels foreach { p =>
      pages += new Page(p.title, p)
      SummaryPanels.this.listenTo(p)
    }
  }

  contents += tabs

  var fight: Fight = EmptyFight()
  var playersAndPets: Set[Actor] = Set.empty
  var summary: Map[Actor, Summary] = Map.empty

  deafTo(this) // otherwise publishing SelectedActorChanged causes StackOverflowError
  listenTo(breakdownButton)
  listenTo(copyDPSButton)
  listenTo(copyHPSButton)

  reactions += {
    case e: SelectedActorChanged => {
      breakdownButton.enabled = true
      if (spellBreakdownDialog.visible) {
        spellBreakdownDialog.update(e.actor, EventProcessor.filterByActors(fight.events, Set(e.actor)))
      }
      publish(e)
    }
    case ButtonClicked(`breakdownButton`) => {
      current.selectedActor match {
        case Some(actor) => {
          spellBreakdownDialog.update(actor, EventProcessor.filterByActors(fight.events, Set(actor)))
          spellBreakdownDialog.visible = true
        }
        case None =>
      }
    }
    case ButtonClicked(`copyDPSButton`) => {
      val clipboard = Toolkit.getDefaultToolkit.getSystemClipboard
      val data = new StringSelection(EventProcessor.dpsSummaryForClipboard(summary))
      clipboard.setContents(data, GUIMain)
    }
    case ButtonClicked(`copyHPSButton`) => {
      val clipboard = Toolkit.getDefaultToolkit.getSystemClipboard
      val data = new StringSelection(EventProcessor.hpsSummaryForClipboard(summary))
      clipboard.setContents(data, GUIMain)
    }
  }

  def current: SummaryPanel = panels(tabs.selection.page.index)
  
  def update(newFight: Fight, newPlayersAndPets: Set[Actor]) {
    fight = newFight
    playersAndPets = newPlayersAndPets
    summary = EventProcessor.summary(fight).filter {
      case (actor, _) => playersAndPets.contains(actor)
    }
    panels foreach { _.update(summary) }
    copyDPSButton.enabled = fight.duration > 0
    copyHPSButton.enabled = fight.duration > 0
  }
}
