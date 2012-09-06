package net.doxxx.universalcombatlog

import scala.swing._
import event._
import java.util.prefs.Preferences
import javax.swing.filechooser.FileNameExtensionFilter
import scala.actors.Actor._
import java.awt.datatransfer.{Transferable, Clipboard, ClipboardOwner}
import java.io.{IOException, File}
import java.text.SimpleDateFormat
import javax.swing.JFileChooser
import java.util

object GUIMain extends SimpleSwingApplication with ClipboardOwner {

  import Utils._

  case class LogFileLoaded(fights: List[Fight], playersAndPets: Set[Actor]) extends Event

  val LogFileKey = "logFile"

  val prefs = Preferences.userNodeForPackage(getClass)

  var logFile = {
    val path = prefs.get(LogFileKey, null)
    if (path == null)
      None
    else
      Some(new File(path))
  }
  var logFileLastModified = 0L

  val logFileEventPublisher = new Publisher {}

  var loadingLogFile = false

  var parser: Option[LogParser] = None

  def chooseCombatLogFile(default: Option[File]): Option[File] = {
    val chooser = new JFileChooser
    chooser.setFileFilter(new FileNameExtensionFilter("Log Files", "txt"))
    chooser.setCurrentDirectory(
      default match {
        case Some(f) => f.getParentFile
        case None => null
      })
    chooser.showOpenDialog(top.self) match {
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
        val date = new SimpleDateFormat("yyyy-MM-dd_HH-mm").format(new util.Date())
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
          top.progressBar.label = "Loading combat log..."
        }

        parser match {
          case Some(p) => {
            actor {
              log("Loading events from %s", f.toString)
              try {
                val events = EventProcessor.normalizeTimes(p.parse(f))
                top.progressBar.label = "Detecting fights..."
                val fights = EventProcessor.splitFights(events).filter(_.duration > 5000)
                logFileLastModified = f.lastModified()
                val playersAndPets = p.playersAndPets
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
          case None => {
            Swing.onEDT {
              top.progressBar.visible = false
            }

            loadingLogFile = false
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

  def createFileExportActor(file: File, fights: List[Fight]) {
    Swing.onEDT {
      top.progressBar.visible = true
      top.progressBar.label = "Exporting UCL file..."
    }

    actor {
      FileConverter.writeUniversalCombatLog(file, fights)

      Swing.onEDT {
        top.progressBar.visible = false
      }
    }
  }

  val top = new MainFrame {
    title = "Universal Combat Log"

    private var _playersAndPets: Set[Actor] = Set.empty

    val fightList = new FightList
    val summaryPanels = new SummaryPanels(prefs)
    def summaryPanel = summaryPanels.current
    val progressBar = new ProgressBar {
      indeterminate = true
      labelPainted = true
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

    EventProcessor.loadSettings(prefs)

    if (prefs.getInt("mainX", 0) > 0) {
      Utils.log("Restoring main window bounds")
      bounds = new Rectangle(prefs.getInt("mainX", 0), prefs.getInt("mainY", 0),
        prefs.getInt("mainW", 0), prefs.getInt("mainH", 0))
    }
    else {
      centerOnScreen()
    }

    val MI_ChooseRiftLogFile = new MenuItem("Choose Rift Log File")
    val MI_ChooseWoWLogFile = new MenuItem("Choose WoW Log File")
    val MI_ExportUCL= new MenuItem("Expot UCL File")
    val MI_NewSession = new MenuItem("New Session")
    val MI_IncludeOverhealing = new CheckMenuItem("Include Overhealing")
    val MI_UseActorCombatTime = new CheckMenuItem("Use Actor Combat Time")
    val MI_MergePetsIntoOwners = new CheckMenuItem("Merge Pets Into Owners")

    MI_IncludeOverhealing.selected = EventProcessor.includeOverhealing
    MI_UseActorCombatTime.selected = EventProcessor.useActorCombatTime
    MI_MergePetsIntoOwners.selected = EventProcessor.mergePetsIntoOwners

    menuBar = new MenuBar {
      contents += new Menu("File") {
        contents += MI_ChooseRiftLogFile
        contents += MI_ChooseWoWLogFile
        contents += MI_ExportUCL
        contents += MI_NewSession
      }
      contents += new Menu("Options") {
        contents += MI_IncludeOverhealing
        contents += MI_UseActorCombatTime
        contents += MI_MergePetsIntoOwners
      }
    }

    listenTo(MI_ChooseRiftLogFile)
    listenTo(MI_ChooseWoWLogFile)
    listenTo(MI_ExportUCL)
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
        summaryPanels.raiseDialogs()
      }
      case WindowDeactivated(_) => {
        summaryPanels.lowerDialogs()
      }
      case WindowClosing(_) => {
        Utils.log("Saving main window bounds")
        prefs.putInt("mainX", bounds.x)
        prefs.putInt("mainY", bounds.y)
        prefs.putInt("mainW", bounds.width)
        prefs.putInt("mainH", bounds.height)
        summaryPanels.saveDialogBounds()
      }
      case ButtonClicked(MI_ChooseRiftLogFile) => {
        logFile = chooseCombatLogFile(logFile)
        logFileLastModified = 0L

        parser match {
          case Some(p: RiftParser) => {
            p.reset()
          }
          case _ => {
            parser = Some(new RiftParser)
          }
        }

        createFileLoaderActor()
      }
      case ButtonClicked(MI_ChooseWoWLogFile) => {
        logFile = chooseCombatLogFile(logFile)
        logFileLastModified = 0L

        parser match {
          case Some(p: WoWParser) => {
            p.reset()
          }
          case _ => {
            parser = Some(new WoWParser)
          }
        }

        createFileLoaderActor()
      }
      case ButtonClicked(MI_ExportUCL) => {
        val default = logFile match {
          case Some(f) => new File(f.getParentFile, f.getName.substring(0, f.getName.indexOf(".txt")) + ".ucl")
          case None => new File("CombatLog.ucl")
        }
        val chooser = new JFileChooser
        chooser.setFileFilter(new FileNameExtensionFilter("UCL Log Files", "ucl"))
        chooser.setCurrentDirectory(default.getParentFile)
        chooser.setDialogTitle("Export UCL File")
        chooser.setApproveButtonText("Export")
        val uclFile = (chooser.showSaveDialog(null) match {
          case JFileChooser.APPROVE_OPTION => {
            val path: String = chooser.getSelectedFile.getPath
            if (!path.toLowerCase.endsWith(".ucl")) {
              new File(path + ".ucl")
            }
            else {
              new File(path)
            }
          }
          case _ => default
        })

        createFileExportActor(uclFile, fightList.selectedFights)
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

