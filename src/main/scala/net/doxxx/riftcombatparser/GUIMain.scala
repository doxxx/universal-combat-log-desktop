package net.doxxx.riftcombatparser

import scala.swing._
import event.{WindowActivated, Event, ButtonClicked}
import io.Source
import java.util.prefs.Preferences
import javax.swing.JFileChooser
import javax.swing.filechooser.FileNameExtensionFilter
import scala.actors.Actor._
import java.awt.Toolkit
import java.awt.datatransfer.{Transferable, Clipboard, ClipboardOwner, StringSelection}
import java.io.{IOException, File}
import java.util.{Date, Calendar}
import java.text.{SimpleDateFormat, DateFormat}

object GUIMain extends SimpleSwingApplication with ClipboardOwner {

  case class LogFileLoaded(events: List[LogEvent], playersAndPets: Set[Actor]) extends Event

  val LogFileKey = "logFile"
  val LoggingDateFormat = DateFormat.getDateTimeInstance(DateFormat.SHORT, DateFormat.LONG)

  val prefs = Preferences.userNodeForPackage(getClass)

  var logFile = {
    val path = prefs.get(LogFileKey, null)
    if (path == null)
      chooseCombatLogFile(None)
    else
      Some(new File(path))
  }
  var logFileLastModified = 0L

  val logFileEventPublisher = new Publisher {}

  def chooseCombatLogFile(default: Option[File]): Option[File] = {
    val chooser = new JFileChooser
    chooser.setFileFilter(new FileNameExtensionFilter("Text Files", "txt"))
    chooser.setCurrentDirectory(
      default match {
        case Some(f) => f.getParentFile
        case None => null
      })
    chooser.showDialog(null, "Choose Combat Log File") match {
      case JFileChooser.APPROVE_OPTION => {
        prefs.put(LogFileKey, chooser.getSelectedFile.getPath)
        Some(new File(chooser.getSelectedFile.getPath))
      }
      case _ => default
    }
  }

  def rolloverCombatLogFile() {
    if (logFile.isDefined) {
      val f = logFile.get
      if (f.exists) {
        val date = new SimpleDateFormat("yyyy-MM-dd_HH-mm").format(new Date())
        f.renameTo(new File(f.getParentFile, f.getName + '.' + date))
      }
    }
  }

  def createFileLoaderActor() {
    logFile match {
      case Some(f) => {
        if (f.lastModified() <= logFileLastModified) return

        actor {
          log("Loading events from %s", f.toString)
          try {
            CombatLogParser.reset()
            val events = EventProcessor.normalizeTimes(CombatLogParser.parse(Source.fromFile(f)))
            logFileLastModified = f.lastModified();
            val playersAndPets = CombatLogParser.playersAndPets
            Swing.onEDT {
              logFileEventPublisher.publish(LogFileLoaded(events, playersAndPets))
            }
          }
          catch {
            case e: IOException => log("Couldn't load combat log file: " + e.toString)
          }
        }
      }
      case None =>
    }
  }

  def createFileWatchActor() {
    logFile match {
      case Some(f) => actor {
        val lastModified = f.lastModified
        try {
          Thread.sleep(5000)
          val modified = f.lastModified
          if (modified > lastModified) {
            createFileLoaderActor()
          }
          createFileWatchActor()
        }
        catch {
          case e: InterruptedException => // nothing
        }
      }
      case None =>
    }
  }

  def top = new MainFrame {
    title = "Rift Combat Parser"

    private var _playersAndPets: Set[Actor] = Set.empty
    private var _summary: Map[Actor, Summary] = Map.empty

    val fightList = new FightList
    val summaryPanels = new SummaryPanels
    def summaryPanel = summaryPanels.current

    val copyDPSButton = new Button("Copy DPS") {
      enabled = false
    }
    val copyHPSButton = new Button("Copy HPS") {
      enabled = false
    }
    val breakdownButton = new Button("Breakdown") {
      enabled = false
    }
    val spellBreakdownDialog = new SpellBreakdownDialog(this)

    val centerComponent = new BoxPanel(Orientation.Vertical) {
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
      }
      contents += Swing.VStrut(5)
      contents += summaryPanels
    }

    contents = new BorderPanel {
      layoutManager.setHgap(5)
      layoutManager.setVgap(5)
      border = Swing.EmptyBorder(5)
      layout(fightList) = BorderPanel.Position.West
      layout(centerComponent) = BorderPanel.Position.Center
    }

    val MI_ChooseCombatLogFile = new MenuItem("Choose Combat Log File")
    val MI_NewSession = new MenuItem("New Session")
    val MI_SpellBreakdown = new MenuItem("Spell Breakdown")
    val MI_CopyDPSSummary = new MenuItem("Copy DPS Summary")
    val MI_CopyHPSSummary = new MenuItem("Copy HPS Summary")
    val MI_IncludeOverhealing = new CheckMenuItem("Include Overhealing")
    val MI_UseActorCombatTime = new CheckMenuItem("Use Actor Combat Time")
    val MI_MergePetsIntoOwners = new CheckMenuItem("Merge Pets Into Owners")

    EventProcessor.loadSettings(prefs)

    MI_IncludeOverhealing.selected = EventProcessor.includeOverhealing
    MI_UseActorCombatTime.selected = EventProcessor.useActorCombatTime
    MI_MergePetsIntoOwners.selected = EventProcessor.mergePetsIntoOwners

    menuBar = new MenuBar {
      contents += new Menu("File") {
        contents += MI_ChooseCombatLogFile
        contents += MI_NewSession
        contents += MI_SpellBreakdown
        contents += MI_CopyDPSSummary
        contents += MI_CopyHPSSummary
      }
      contents += new Menu("Options") {
        contents += MI_IncludeOverhealing
        contents += MI_UseActorCombatTime
        contents += MI_MergePetsIntoOwners
      }
    }

    listenTo(MI_ChooseCombatLogFile)
    listenTo(MI_NewSession)
    listenTo(MI_SpellBreakdown)
    listenTo(MI_CopyDPSSummary)
    listenTo(MI_CopyHPSSummary)
    listenTo(MI_IncludeOverhealing)
    listenTo(MI_UseActorCombatTime)
    listenTo(MI_MergePetsIntoOwners)
    listenTo(logFileEventPublisher)
    listenTo(fightList)
    listenTo(summaryPanels)
    listenTo(copyDPSButton)
    listenTo(copyHPSButton)
    listenTo(breakdownButton)

    reactions += {
      case WindowActivated(_) => {
        createFileLoaderActor()
      }
      case ButtonClicked(MI_ChooseCombatLogFile) => {
        logFile = chooseCombatLogFile(logFile)
        logFileLastModified = 0L
        createFileLoaderActor()
        //createFileWatchActor()
      }
      case ButtonClicked(MI_NewSession) => {
        rolloverCombatLogFile()
        fightList.update(Nil)
      }
      case ButtonClicked(MI_SpellBreakdown) => {
        summaryPanel.selectedActor match {
          case Some(actor) => {
            val combined = Fights(fightList.selectedFights)
            spellBreakdownDialog.update(actor, EventProcessor.filterByActors(combined.events, Set(actor)))
            spellBreakdownDialog.visible = true
          }
          case None =>
        }
      }
      case ButtonClicked(MI_CopyDPSSummary) => {
        val clipboard = Toolkit.getDefaultToolkit.getSystemClipboard
        val data = new StringSelection(EventProcessor.dpsSummaryForClipboard(_summary))
        clipboard.setContents(data, GUIMain)
      }
      case ButtonClicked(MI_CopyHPSSummary) => {
        val clipboard = Toolkit.getDefaultToolkit.getSystemClipboard
        val data = new StringSelection(EventProcessor.hpsSummaryForClipboard(_summary))
        clipboard.setContents(data, GUIMain)
      }
      case ButtonClicked(MI_IncludeOverhealing) => {
        EventProcessor.includeOverhealing = MI_IncludeOverhealing.selected
        EventProcessor.saveSettings(prefs)
        fightList.fireSelectedFightsChanged()
      }
      case ButtonClicked(MI_UseActorCombatTime) => {
        EventProcessor.useActorCombatTime = MI_UseActorCombatTime.selected
        EventProcessor.saveSettings(prefs)
        fightList.fireSelectedFightsChanged()
      }
      case ButtonClicked(MI_MergePetsIntoOwners) => {
        EventProcessor.mergePetsIntoOwners = MI_MergePetsIntoOwners.selected
        EventProcessor.saveSettings(prefs)
        fightList.fireSelectedFightsChanged()
      }
      case ButtonClicked(`breakdownButton`) => {
        summaryPanel.selectedActor match {
          case Some(actor) => {
            val combined = Fights(fightList.selectedFights)
            spellBreakdownDialog.update(actor, EventProcessor.filterByActors(combined.events, Set(actor)))
            spellBreakdownDialog.visible = true
          }
          case None =>
        }
      }
      case ButtonClicked(`copyDPSButton`) => {
        val clipboard = Toolkit.getDefaultToolkit.getSystemClipboard
        val data = new StringSelection(EventProcessor.dpsSummaryForClipboard(_summary))
        clipboard.setContents(data, GUIMain)
      }
      case ButtonClicked(`copyHPSButton`) => {
        val clipboard = Toolkit.getDefaultToolkit.getSystemClipboard
        val data = new StringSelection(EventProcessor.hpsSummaryForClipboard(_summary))
        clipboard.setContents(data, GUIMain)
      }
      case LogFileLoaded(events, playersAndPets) => {
        fightList.update(events)
        _playersAndPets = playersAndPets
      }
      case SelectedFightsChanged(fights) => {
        val combined = Fights(fights)
        _summary = EventProcessor.summary(combined).filter {
          case (actor, summary) => _playersAndPets.contains(actor)
        }
        summaryPanels.update(_summary)
        if (fights.size > 0) {
          copyDPSButton.enabled = true
          copyHPSButton.enabled = true
        }
      }
      case SelectedActorChanged(actor) => {
        if (spellBreakdownDialog.visible) {
          val events = (for (f <- fightList.selectedFights) yield f.events).flatten
          spellBreakdownDialog.update(actor, EventProcessor.filterByActors(events, Set(actor)))
        }
        breakdownButton.enabled = true
      }
    }

    createFileLoaderActor()
    //createFileWatchActor()
  }

  def log(msg: String, args: Any*) {
    val a = formattedDate :: args.toList
    printf("%s: " + msg + "\n", a: _*)
  }

  def formattedDate = {
    LoggingDateFormat.format(Calendar.getInstance.getTime)
  }

  def lostOwnership(clipboard: Clipboard, contents: Transferable) {
    // do nothing
  }
}

