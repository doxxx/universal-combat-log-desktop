package net.doxxx.riftcombatparser

import swing._
import event.{SelectionChanged, ButtonClicked}
import swing.TabbedPane.Page
import net.doxxx.riftcombatparser.SummaryColumns._
import java.awt.Toolkit
import java.awt.datatransfer.StringSelection

class SummaryPanels extends BoxPanel(Orientation.Vertical) {

  val panels = Seq(
    new SummaryPanel("Overview", Seq(Name, DPSOut, HPSOut, Deaths, CombatTime), DPSOut),
    new SummaryPanel("Damage Out", Seq(Name, DPSOut, DamageOut), DPSOut),
    new SummaryPanel("Healing Out", Seq(Name, HPSOut, HealingOut, Overhealing), HPSOut),
    new SummaryPanel("Damage In", Seq(Name, DPSIn, DamageIn), DamageIn),
    new SummaryPanel("Healing In", Seq(Name, HPSIn, HealingIn), HealingIn)
  )

  val targetDropdown = new ComboBox[String](Nil) {
    implicit val order = new Ordering[Actor] {
      def compare(x: Actor, y: Actor):Int = x.name.compareTo(y.name)
    }
    var actors: Seq[Actor] = Nil
    def setItems(items: Seq[Actor]) {
      actors = items.sorted
      val actorNames:List[String] = actors.map(_.name).toList
      peer.setModel(ComboBox.newConstantModel("**ALL**" :: actorNames))
    }
    def selectedActor: Option[Actor] = {
      actors.find(_.name == selection.item)
    }
    def selectActor(actor: Option[Actor]) {
      actor match {
        case None => selection.item = "**ALL**"
        case Some(a) => selection.item = a.name
      }
    }
  }
  val copyDPSButton = new Button("Copy DPS") {
    enabled = false
  }
  val copyHPSButton = new Button("Copy HPS") {
    enabled = false
  }
  val breakdownDialog = new BreakdownDialog(GUIMain.top)

  contents += new BoxPanel(Orientation.Horizontal) {
    contents += new Label {
      text = "Summary"
    }
    contents += Swing.HGlue
    contents += targetDropdown
    contents += Swing.HStrut(5)
    contents += copyDPSButton
    contents += Swing.HStrut(5)
    contents += copyHPSButton
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
  listenTo(copyDPSButton)
  listenTo(copyHPSButton)
  listenTo(targetDropdown.selection)

  reactions += {
    case e: SelectedActorChanged => {
      if (breakdownDialog.visible) {
        breakdownDialog.update(e.actor)
      }
      publish(e)
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
    case SelectionChanged(`targetDropdown`) => {
      targetDropdown.selectedActor match {
        case None => {
          resetTargetFilter()
        }
        case Some(a) => {
          applyTargetFilter(a)
        }
      }
    }
    case BreakdownRequested(actor, breakdownType) => {
      breakdownDialog.update(actor, breakdownType, fight.events)
      breakdownDialog.visible = true
    }
  }

  def current: SummaryPanel = panels(tabs.selection.page.index)

  def update(newFight: Fight, newPlayersAndPets: Set[Actor]) {
    fight = newFight
    playersAndPets = newPlayersAndPets
    summary = EventProcessor.summary(fight).filter {
      case (actor, _) => playersAndPets.contains(actor)
    }
    panels foreach { _.update(summary)}
    val oldTarget = targetDropdown.selectedActor
    targetDropdown.setItems(EventProcessor.actorsSortedByActivity(newFight.events))
    targetDropdown.selectActor(oldTarget)
    copyDPSButton.enabled = fight.duration > 0
    copyHPSButton.enabled = fight.duration > 0
    if (breakdownDialog.visible) {
      current.selectedActor match {
        case Some(actor) => breakdownDialog.update(actor, breakdownDialog.currentBreakdownType, fight.events)
        case None => breakdownDialog.visible = false
      }
    }
  }

  private def applyTargetFilter(target: Actor) {
    val filtered = SingleFight(EventProcessor.filterByTarget(fight.events, target))
    summary = EventProcessor.summary(filtered).filter {
      case (actor, _) => playersAndPets.contains(actor)
    }
    panels foreach { _.update(summary)}
  }

  private def resetTargetFilter() {
    summary = EventProcessor.summary(fight).filter {
      case (actor, _) => playersAndPets.contains(actor)
    }
    panels foreach { _.update(summary)}
  }

}
