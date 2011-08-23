package net.doxxx.riftcombatparser

import scala.swing._
import event.{WindowActivated, Event, ButtonClicked}
import java.util.prefs.Preferences
import javax.swing.filechooser.FileNameExtensionFilter
import scala.actors.Actor._
import java.awt.datatransfer.{Transferable, Clipboard, ClipboardOwner}
import java.io.{IOException, File}
import java.util.Date
import java.text.SimpleDateFormat
import javax.swing.JFileChooser

object GUIMain extends SimpleSwingApplication with ClipboardOwner {

  import Utils._

  case class LogFileLoaded(fights: List[Fight], playersAndPets: Set[Actor]) extends Event

  val LogFileKey = "logFile"

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

  var loadingLogFile = false

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

        loadingLogFile = true

        Swing.onEDT {
          top.progressBar.visible = true
        }

        actor {
          log("Loading events from %s", f.toString)
          try {
            val events = EventProcessor.normalizeTimes(CombatLogParser.parse(f))
            val fights = EventProcessor.splitFights(events)
            logFileLastModified = f.lastModified();
            val playersAndPets = CombatLogParser.playersAndPets
            Swing.onEDT {
              logFileEventPublisher.publish(LogFileLoaded(fights, playersAndPets))
            }
          }
          catch {
            case e: IOException => log("Couldn't load combat log file: " + e.toString)
          }

          Swing.onEDT {
            top.progressBar.visible = false
          }

          loadingLogFile = false
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

  val top = new MainFrame {
    title = "Rift Combat Parser"

    private var _playersAndPets: Set[Actor] = Set.empty

    val fightList = new FightList
    val summaryPanels = new SummaryPanels
    def summaryPanel = summaryPanels.current
    val progressBar = new ProgressBar {
      indeterminate = true
      labelPainted = true
      label = "Parsing log file..."
      visible = false
    }

    contents = new BorderPanel {
      layoutManager.setHgap(5)
      layoutManager.setVgap(5)
      border = Swing.EmptyBorder(5)
      layout(fightList) = BorderPanel.Position.West
      layout(summaryPanels) = BorderPanel.Position.Center
      layout(progressBar) = BorderPanel.Position.South
    }

    val MI_ChooseCombatLogFile = new MenuItem("Choose Combat Log File")
    val MI_NewSession = new MenuItem("New Session")
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
      }
      contents += new Menu("Options") {
        contents += MI_IncludeOverhealing
        contents += MI_UseActorCombatTime
        contents += MI_MergePetsIntoOwners
      }
    }

    listenTo(MI_ChooseCombatLogFile)
    listenTo(MI_NewSession)
    listenTo(MI_IncludeOverhealing)
    listenTo(MI_UseActorCombatTime)
    listenTo(MI_MergePetsIntoOwners)
    listenTo(logFileEventPublisher)
    listenTo(fightList)
    listenTo(summaryPanels)

    reactions += {
      case WindowActivated(_) => {
        if (!loadingLogFile) {
          createFileLoaderActor()
        }
      }
      case ButtonClicked(MI_ChooseCombatLogFile) => {
        logFile = chooseCombatLogFile(logFile)
        logFileLastModified = 0L
        CombatLogParser.reset()
        createFileLoaderActor()
      }
      case ButtonClicked(MI_NewSession) => {
        rolloverCombatLogFile()
        fightList.update(Nil)
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
      case LogFileLoaded(fights, playersAndPets) => {
        fightList.update(fights)
        _playersAndPets = playersAndPets
      }
      case SelectedFightsChanged(fights) => {
        val combined = Fights(fights)
        summaryPanels.update(combined, _playersAndPets)
      }
    }
  }

  def lostOwnership(clipboard: Clipboard, contents: Transferable) {
    // do nothing
  }
}

