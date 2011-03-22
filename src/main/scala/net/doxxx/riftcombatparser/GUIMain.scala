package net.doxxx.riftcombatparser

import scala.swing._
import event.ButtonClicked
import io.Source
import java.util.prefs.Preferences
import javax.swing.JFileChooser
import javax.swing.filechooser.FileNameExtensionFilter
import java.io.File

object GUIMain extends SimpleSwingApplication {
  val LogFileKey = "logFile"

  val prefs = Preferences.userNodeForPackage(getClass)

  def chooseCombatLogFile(default: Option[String]): Option[String] = {
    val chooser = new JFileChooser
    chooser.setFileFilter(new FileNameExtensionFilter("Text Files", "txt"))
    chooser.setCurrentDirectory(
      default match {
        case Some(path) => new File(path)
        case None => null
      })
    chooser.showDialog(null, "Choose Combat Log File") match {
      case JFileChooser.APPROVE_OPTION => {
        prefs.put(LogFileKey, chooser.getSelectedFile.getPath)
        Some(chooser.getSelectedFile.getPath)
      }
      case _ => default
    }
  }

  var logFile = {
    val path = prefs.get(LogFileKey, null)
    if (path == null) {
      chooseCombatLogFile(None)
    }
    else Some(path)
  }

  def createSummaryPanel(logFile: Option[String]) = {
    val events = logFile match {
      case Some(path) => new Parser(Source.fromFile(path)).parse()
      case _ => Nil
    }
    new SummaryPanel(events)
  }
  
  def top = new MainFrame {
    title = "Rift Combat Parser"
    contents = createSummaryPanel(logFile)

    val MI_ChooseCombatLogFile = new MenuItem("Choose Combat Log File")

    menuBar = new MenuBar {
      contents += new Menu("Rift Combat Parser") {
        contents += MI_ChooseCombatLogFile
      }
    }

    listenTo(MI_ChooseCombatLogFile)

    reactions += {
      case ButtonClicked(MI_ChooseCombatLogFile) => contents = {
        logFile = chooseCombatLogFile(logFile)
        createSummaryPanel(logFile)
      }
    }
  }
}
