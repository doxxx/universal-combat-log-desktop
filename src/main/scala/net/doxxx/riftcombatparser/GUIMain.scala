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
import java.io.{FileReader, File}

object GUIMain extends SimpleSwingApplication {

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
        val events = new CombatLogParser(Source.fromFile(f)).events
        Swing.onEDT {
          logFileEventPublisher.publish(UpdateWithEvents(events))
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

    val summaryPanel = new SummaryPanel
    val actorList = new ActorList

    contents = new BorderPanel {
      layoutManager.setHgap(5)
      layoutManager.setVgap(5)
      layout(summaryPanel) = BorderPanel.Position.Center
      layout(actorList) = BorderPanel.Position.East
    }

    val MI_ChooseCombatLogFile = new MenuItem("Choose Combat Log File")
    val MI_LoadActorsFromRaidXML = new MenuItem("Load Actors From raid.xml")

    menuBar = new MenuBar {
      contents += new Menu("Rift Combat Parser") {
        contents += MI_ChooseCombatLogFile
        contents += MI_LoadActorsFromRaidXML
      }
    }

    listenTo(MI_ChooseCombatLogFile)
    listenTo(MI_LoadActorsFromRaidXML)
    listenTo(logFileEventPublisher)
    listenTo(actorList)

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
      case UpdateWithEvents(events) => {
        summaryPanel.updateEvents(EventProcessor.summary(events))
        actorList.update(EventProcessor.actors(events))
      }
      case SelectedActorsChanged(actors) => {
        summaryPanel.applyActorFilter(actors)
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
}

