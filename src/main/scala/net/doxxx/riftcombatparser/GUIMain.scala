package net.doxxx.riftcombatparser

import scala.swing._
import scala.swing.event.{Event, ButtonClicked}
import io.Source
import java.util.prefs.Preferences
import javax.swing.JFileChooser
import javax.swing.filechooser.FileNameExtensionFilter
import scala.actors.Actor._
import java.util.Calendar
import java.text.DateFormat
import java.awt.Toolkit
import java.awt.datatransfer.{Transferable, Clipboard, ClipboardOwner, StringSelection}
import java.io.{IOException, FileReader, File}

object GUIMain extends SimpleSwingApplication with ClipboardOwner {

  case class UpdateWithEvents(events: List[LogEvent]) extends Event

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

  val logFileEventPublisher = new Publisher {}

  def chooseCombatLogFile(default: Option[File]): Option[File] = {
    val chooser = new JFileChooser
    chooser.setFileFilter(new FileNameExtensionFilter("Text Files", "txt"))
    chooser.setCurrentDirectory(
      default match {
        case Some(f) => f
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

  def createFileLoaderActor() {
    logFile match {
      case Some(f) => actor {
        log("Loading events from %s", f.toString)
        try {
          val events = EventProcessor.normalizeTimes(CombatLogParser.parse(Source.fromFile(f)))
          Swing.onEDT {
            logFileEventPublisher.publish(UpdateWithEvents(events))
          }
        }
        catch {
          case e: IOException => log("Couldn't load combat log file: " + e.toString)
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

    val summaryPanels = new SummaryPanels
    val actorList = new ActorList
    val fightList = new FightList

    var fullSummary: Map[String, Summary] = Map.empty
    var filteredSummary: Option[Map[String, Summary]] = None
    def summary = filteredSummary getOrElse fullSummary

    def summaryPanel = summaryPanels.current

    val spellBreakdownDialog = new SpellBreakdownDialog(this)

    contents = new BoxPanel(Orientation.Vertical) {
      contents += Swing.VStrut(5)
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += Swing.HStrut(5)
        contents += fightList
        contents += Swing.HStrut(5)
        contents += summaryPanels
        contents += Swing.HStrut(5)
        contents += actorList
        contents += Swing.HStrut(5)
      }
      contents += Swing.VStrut(5)
    }

    val MI_ChooseCombatLogFile = new MenuItem("Choose Combat Log File")
    val MI_LoadActorsFromRaidXML = new MenuItem("Load Actors From raid.xml")
    val MI_SpellBreakdown = new MenuItem("Spell Breakdown")
    val MI_CopyDPSSummary = new MenuItem("Copy DPS Summary")
    val MI_CopyHPSSummary = new MenuItem("Copy HPS Summary")
    val MI_IncludeOverhealing = new CheckMenuItem("Include Overhealing")
    val MI_UseActorCombatTime = new CheckMenuItem("Use Actor Combat Time")

    menuBar = new MenuBar {
      contents += new Menu("File") {
        contents += MI_ChooseCombatLogFile
        contents += MI_LoadActorsFromRaidXML
        contents += MI_SpellBreakdown
        contents += MI_CopyDPSSummary
        contents += MI_CopyHPSSummary
      }
      contents += new Menu("Options") {
        contents += MI_IncludeOverhealing
        contents += MI_UseActorCombatTime
      }
    }

    listenTo(MI_ChooseCombatLogFile)
    listenTo(MI_LoadActorsFromRaidXML)
    listenTo(MI_SpellBreakdown)
    listenTo(MI_CopyDPSSummary)
    listenTo(MI_CopyHPSSummary)
    listenTo(MI_IncludeOverhealing)
    listenTo(MI_UseActorCombatTime)
    listenTo(logFileEventPublisher)
    listenTo(actorList)
    listenTo(fightList)
    listenTo(summaryPanels)

    reactions += {
      case ButtonClicked(MI_ChooseCombatLogFile) => {
        logFile = chooseCombatLogFile(logFile)
        createFileLoaderActor()
        createFileWatchActor()
      }
      case ButtonClicked(MI_LoadActorsFromRaidXML) => {
        logFile match {
          case Some(f) => {
            val raidXMLFile = new File(f.getParentFile, "raid.xml")
            if (raidXMLFile.exists) {
              val raidInfo = new RaidInfoParser(new FileReader(raidXMLFile)).parse()
              log("Loading actors from %s", raidXMLFile.toString)
              actorList.selectActors(raidInfo.members.map(_.name).toSet)
            } else {
              println("No raid.xml to load")
            }
          }
          case None => println("No combat log file to locate raid.xml")
        }
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
        val data = new StringSelection(EventProcessor.dpsSummaryForClipboard(summary))
        clipboard.setContents(data, GUIMain)
      }
      case ButtonClicked(MI_CopyHPSSummary) => {
        val clipboard = Toolkit.getDefaultToolkit.getSystemClipboard
        val data = new StringSelection(EventProcessor.hpsSummaryForClipboard(summary))
        clipboard.setContents(data, GUIMain)
      }
      case ButtonClicked(MI_IncludeOverhealing) => {
        EventProcessor.includeOverhealing = MI_IncludeOverhealing.selected
        fightList.fireSelectedFightsChanged()
      }
      case ButtonClicked(MI_UseActorCombatTime) => {
        EventProcessor.useActorCombatTime = MI_UseActorCombatTime.selected
        fightList.fireSelectedFightsChanged()
      }
      case UpdateWithEvents(events) => {
        fightList.update(events)
      }
      case SelectedFightsChanged(fights) => {
        val combined = Fights(fights)
        fullSummary = EventProcessor.summary(combined)
        actorList.update(EventProcessor.actorsSortedByActivity(combined.events))
      }
      case ActorFilterChanged(actors) => {
        filteredSummary = Some(EventProcessor.filterSummaryByActors(fullSummary, actors))
        summaryPanels.update(summary)
      }
      case SelectedActorChanged(actor) => {
        if (spellBreakdownDialog.visible) {
          val events = (for (f <- fightList.selectedFights) yield f.events).flatten
          spellBreakdownDialog.update(actor, EventProcessor.filterByActors(events, Set(actor)))
        }
      }
    }

    createFileLoaderActor()
    createFileWatchActor()
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

