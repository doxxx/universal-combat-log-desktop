package net.doxxx.riftcombatparser

import scala.swing._
import scala.swing.event.{Event => SEevent, ButtonClicked}
import io.Source
import java.util.prefs.Preferences
import javax.swing.JFileChooser
import javax.swing.filechooser.FileNameExtensionFilter
import java.io.File
import scala.actors.Actor._

object GUIMain extends SimpleSwingApplication {
  val LogFileKey = "logFile"

  val prefs = Preferences.userNodeForPackage(getClass)

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

  var logFile = {
    val path = prefs.get(LogFileKey, null)
    if (path == null) {
      chooseCombatLogFile(None)
    }
    else Some(new File(path))
  }

  def parseLogFile(logFile: Option[File]) = {
    logFile match {
      case Some(path) => new Parser(Source.fromFile(path)).parse()
      case _ => Nil
    }
  }

  def createSummaryPanel() = {
    new SummaryPanel(parseLogFile(logFile))
  }

  val fileWatcherPublisher = new Publisher {}

  def createFileWatchActor() {
    logFile match {
      case Some(f) => actor {
        val lastModified = f.lastModified
        try {
          Thread.sleep(5000)
        }
        catch {
          case e: InterruptedException => return
        }
        val modified = f.lastModified
        if (modified > lastModified) {
          Swing.onEDT {
            fileWatcherPublisher.publish(CombatLogFileChanged())
          }
        }
        createFileWatchActor()
      }
      case None =>
    }
  }

  createFileWatchActor()

  def top = new MainFrame {
    title = "Rift Combat Parser"

    val summaryPanel = new SummaryPanel(parseLogFile(logFile))

    contents = summaryPanel

    val MI_ChooseCombatLogFile = new MenuItem("Choose Combat Log File")

    menuBar = new MenuBar {
      contents += new Menu("Rift Combat Parser") {
        contents += MI_ChooseCombatLogFile
      }
    }

    listenTo(MI_ChooseCombatLogFile)
    listenTo(fileWatcherPublisher)

    reactions += {
      case ButtonClicked(MI_ChooseCombatLogFile) => {
        logFile = chooseCombatLogFile(logFile)
        createFileWatchActor()
        summaryPanel.updateEvents(parseLogFile(logFile))
      }
      case CombatLogFileChanged() => {
        println("Reloading combat log file")
        summaryPanel.updateEvents(parseLogFile(logFile))
      }
    }
  }
}

case class CombatLogFileChanged() extends SEevent
