package net.doxxx.universalcombatlog

import scala.swing._
import event._
import java.util.prefs.Preferences
import scala.actors.Actor._
import java.awt.datatransfer.{Transferable, Clipboard, ClipboardOwner}
import java.io.{IOException, File}
import java.text.SimpleDateFormat
import javax.swing.{KeyStroke, JOptionPane, UIManager}
import java.util
import java.awt.{Toolkit, FileDialog}
import java.awt.event.KeyEvent

object GUIMain extends SimpleSwingApplication with ClipboardOwner {

  import Utils._

  System.setProperty("apple.laf.useScreenMenuBar", "true")
  System.setProperty("com.apple.mrj.application.apple.menu.about.name", "Universal Combat Log")
  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)

  case class LogFileLoaded(fights: List[Fight], playersAndPets: Set[Actor]) extends Event

  val prefs = Preferences.userNodeForPackage(getClass)

  val networkService = new NetworkService()
  networkService.start()

  var logFile: Option[File] = None
  var logFileLastModified = 0L

  val logFileEventPublisher = new Publisher {}

  var loadingLogFile = false

  var parser: Option[LogParser] = None

  def chooseFile(title: String, mode: Int, default: Option[File] = None): Option[File] = {
    val dialog = new FileDialog(top.peer, title, mode)
    default match {
      case Some(f) => dialog.setFile(f.toString)
      case None => // nothing
    }
    dialog.setVisible(true)
    dialog.getFile match {
      case s: String => Some(new File(s))
      case null => None
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
                createFileWatchActor()
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

    val MI_OpenLogFile = new MenuItem("Open...")
    val MI_ExportUCL= new MenuItem("Export UCL File...")
    val MI_NewSession = new MenuItem("New Session")
    val MI_IncludeOverhealing = new CheckMenuItem("Include Overhealing")
    val MI_UseActorCombatTime = new CheckMenuItem("Use Actor Combat Time")
    val MI_MergePetsIntoOwners = new CheckMenuItem("Merge Pets Into Owners")

    val shortcutKey = Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()
    MI_OpenLogFile.peer.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_O, shortcutKey))
    MI_ExportUCL.peer.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_E, shortcutKey))
    MI_NewSession.peer.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_N, shortcutKey))

    MI_IncludeOverhealing.selected = EventProcessor.includeOverhealing
    MI_UseActorCombatTime.selected = EventProcessor.useActorCombatTime
    MI_MergePetsIntoOwners.selected = EventProcessor.mergePetsIntoOwners

    menuBar = new MenuBar {
      contents += new Menu("File") {
        contents += MI_OpenLogFile
        contents += MI_ExportUCL
        contents += MI_NewSession
      }
      contents += new Menu("Options") {
        contents += MI_IncludeOverhealing
        contents += MI_UseActorCombatTime
        contents += MI_MergePetsIntoOwners
      }
    }

    listenTo(MI_OpenLogFile)
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
      case ButtonClicked(MI_OpenLogFile) => {
        chooseFile("Open Log File", FileDialog.LOAD) match {
          case Some(f) => {
            logFileLastModified = 0L
            logFile = Some(f)
            parser = LogParser.detectFormat(f)
            parser match {
              case Some(p) => createFileLoaderActor()
              case _ => JOptionPane.showMessageDialog(self, "Could not determine log file format.",
                "Open Log File", JOptionPane.ERROR_MESSAGE)
            }

          }
          case None => // do nothing
        }
      }
      case ButtonClicked(MI_ExportUCL) => {
        val default = logFile match {
          case Some(f) => Some(new File(f.getParentFile, f.getName.substring(0, f.getName.indexOf(".txt")) + ".ucl"))
          case None => Some(new File("CombatLog.ucl"))
        }
        chooseFile("Export UCL File", FileDialog.SAVE, default) match {
          case Some(f) => {
            val path: String = f.getPath
            val uclFile = if (!path.toLowerCase.endsWith(".ucl")) {
              new File(path + ".ucl")
            }
            else {
              new File(path)
            }
            createFileExportActor(uclFile, fightList.selectedFights)
          }
          case None =>
        }
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
        networkService.fights = fights
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

