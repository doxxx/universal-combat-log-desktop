package net.doxxx.universalcombatlog.gui

import swing._
import event.{SelectionChanged, ButtonClicked}
import swing.TabbedPane.Page
import SummaryColumns._
import java.awt.datatransfer.StringSelection
import java.util.prefs.Preferences
import java.awt.{Rectangle, Toolkit}
import net.doxxx.universalcombatlog._

class SummaryPanels(prefs: Preferences) extends BoxPanel(Orientation.Vertical) {

  val panels = Seq(
    new SummaryPanel("Overview", Seq(Name, DPSOut, HPSOut, Deaths, CombatTime), DPSOut),
    new SummaryPanel("Damage Out", Seq(Name, DPSOut, DamageOut), DPSOut),
    new SummaryPanel("Healing Out", Seq(Name, HPSOut, HealingOut, Overhealing), HPSOut),
    new SummaryPanel("Damage In", Seq(Name, DPSIn, DamageIn), DamageIn),
    new SummaryPanel("Healing In", Seq(Name, HPSIn, HealingIn), HealingIn)
  )

  val targetDropdown = new ComboBox[String](Nil) {
    implicit val order = new Ordering[Entity] {
      def compare(x: Entity, y: Entity):Int = x.name.compareTo(y.name)
    }
    var actors: Seq[Entity] = Nil
    def setItems(items: Seq[Entity]) {
      actors = items.sorted
      val actorNames:List[String] = actors.map(_.name).toList
      peer.setModel(ComboBox.newConstantModel("**ALL**" :: actorNames))
    }
    def selectedActor: Option[Entity] = {
      actors.find(_.name == selection.item)
    }
    def selectActor(actor: Option[Entity]) {
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
  val breakdownDialog = new BreakdownDialog(Main.top) {
    peer.setAlwaysOnTop(true)
    peer.setFocusableWindowState(false)
    if (prefs.getInt("breakdownDialogX", 0) > 0) {
      Utils.log("Restoring breakdown dialog bounds")
      bounds = new Rectangle(prefs.getInt("breakdownDialogX", 0), prefs.getInt("breakdownDialogY", 0),
        prefs.getInt("breakdownDialogW", 0), prefs.getInt("breakdownDialogH", 0))
    }
    else {
      centerOnScreen()
    }
  }
  val deathLogDialog = new DeathLogDialog(Main.top) {
    peer.setAlwaysOnTop(true)
    peer.setFocusableWindowState(false)
    if (prefs.getInt("deathLogDialogX", 0) > 0) {
      Utils.log("Restoring breakdown dialog pos")
      bounds = new Rectangle(prefs.getInt("deathLogDialogX", 0), prefs.getInt("deathLogDialogY", 0),
        prefs.getInt("deathLogDialogW", 0), prefs.getInt("deathLogDialogH", 0))
    }
    else {
      centerOnScreen()
    }
  }

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
  var playersAndPets: Set[Entity] = Set.empty
  var summary: Map[Entity, Summary] = Map.empty

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
      clipboard.setContents(data, Main)
    }
    case ButtonClicked(`copyHPSButton`) => {
      val clipboard = Toolkit.getDefaultToolkit.getSystemClipboard
      val data = new StringSelection(EventProcessor.hpsSummaryForClipboard(summary))
      clipboard.setContents(data, Main)
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
    case DeathLogRequested(actor) => {
      deathLogDialog.update(actor, fight.events)
      deathLogDialog.visible = true
    }
  }

  def current: SummaryPanel = panels(tabs.selection.page.index)

  def update(newFight: Fight, newPlayersAndPets: Set[Entity]) {
    fight = newFight
    playersAndPets = newPlayersAndPets
    summary = EventProcessor.summary(fight, playersAndPets)
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

  def raiseDialogs() {
    breakdownDialog.peer.setAlwaysOnTop(true)
    deathLogDialog.peer.setAlwaysOnTop(true)
  }

  def lowerDialogs() {
    breakdownDialog.peer.setAlwaysOnTop(false)
    deathLogDialog.peer.setAlwaysOnTop(false)
  }

  def saveDialogBounds() {
    Utils.log("Saving death log dialog bounds")
    prefs.putInt("deathLogDialogX", deathLogDialog.bounds.x)
    prefs.putInt("deathLogDialogY", deathLogDialog.bounds.y)
    prefs.putInt("deathLogDialogW", deathLogDialog.bounds.width)
    prefs.putInt("deathLogDialogH", deathLogDialog.bounds.height)

    Utils.log("Saving breakdown dialog bounds")
    prefs.putInt("breakdownDialogX", breakdownDialog.bounds.x)
    prefs.putInt("breakdownDialogY", breakdownDialog.bounds.y)
    prefs.putInt("breakdownDialogW", breakdownDialog.bounds.width)
    prefs.putInt("breakdownDialogH", breakdownDialog.bounds.height)
  }

  private def applyTargetFilter(target: Entity) {
    val filtered = SingleFight(EventProcessor.filterByTarget(fight.events, target))
    summary = EventProcessor.summary(filtered, playersAndPets)
    panels foreach { _.update(summary)}
  }

  private def resetTargetFilter() {
    summary = EventProcessor.summary(fight, playersAndPets)
    panels foreach { _.update(summary)}
  }

}
