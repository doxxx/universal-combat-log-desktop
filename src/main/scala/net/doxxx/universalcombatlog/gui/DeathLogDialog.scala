package net.doxxx.universalcombatlog.gui

import swing._
import event.ListSelectionChanged
import net.doxxx.universalcombatlog._

class DeathLogDialog(owner: Window) extends Dialog(owner) {
  var entity: Entity = Nobody
  var events: List[LogEvent] = Nil

  val deathsList = new ListView[ListEntry] {
    selection.intervalMode = ListView.IntervalMode.Single
  }

  val deathLog = new TextArea {
    editable = false
  }

  val healthGraph = new Graph

  contents = new BorderPanel {
    val deathsListScroller = new ScrollPane(deathsList) {
      preferredSize = new Dimension(300,500)
    }
    layout(deathsListScroller) = BorderPanel.Position.West
    val deathLogScroller = new ScrollPane(deathLog) {
      preferredSize = new Dimension(600,500)
    }
    layout(deathLogScroller) = BorderPanel.Position.Center
    layout(healthGraph) = BorderPanel.Position.South
  }

  listenTo(deathsList.selection)

  reactions += {
    case ListSelectionChanged(source, range, live) => {
      val selectedItems = deathsList.selection.items
      if (!live && selectedItems.size > 0) {
        val deathEvent = selectedItems(0).event
        val preDeathEvents = EventProcessor.eventsUpToDeath(deathEvent, events, EventTypes.DamageTypes ++ EventTypes.HealTypes)
        deathLog.text = preDeathEvents.map {
          case ce: CombatEvent => Some("%.1f> %s".format((ce.time - events.head.time) / 1000.0, ce.text))
          case _ => None
        }.flatten.mkString("\n")
        healthGraph.data = EventProcessor.chartHealthPriorToDeath(entity, preDeathEvents)
      }
    }
  }

  def update(entity: Entity, events: List[LogEvent]) {
    this.entity = entity
    this.events = events
    deathsList.listData = EventProcessor.entityDeaths(entity, events) map { e => ListEntry(e, e.time-events.head.time, e.text) }
    if (deathsList.listData.isEmpty) {
      deathLog.text = ""
      healthGraph.data = Array.empty
    }
    else {
      deathsList.selectIndices(0)
    }
    pack()
  }

  case class ListEntry(event: CombatEvent, time: Long, text: String) {
    override def toString = "%.1f> %s".format(time / 1000.0, text)
  }
}
