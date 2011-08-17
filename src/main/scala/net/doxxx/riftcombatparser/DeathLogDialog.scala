package net.doxxx.riftcombatparser

import swing._
import event.ListSelectionChanged

class DeathLogDialog(owner: Window) extends Dialog(owner) {
  var events: List[LogEvent] = Nil

  val deathsList = new ListView[ListEntry] {
    selection.intervalMode = ListView.IntervalMode.Single
  }

  val deathLog = new TextArea {
    editable = false
  }

  contents = new BorderPanel {
    val deathsListScroller = new ScrollPane(deathsList) {
      preferredSize = new Dimension(300,500)
    }
    layout(deathsListScroller) = BorderPanel.Position.West
    val deathLogScroller = new ScrollPane(deathLog) {
      preferredSize = new Dimension(600,500)
    }
    layout(deathLogScroller) = BorderPanel.Position.Center
  }

  listenTo(deathsList.selection)

  reactions += {
    case ListSelectionChanged(source, range, live) => {
      val selectedItems = deathsList.selection.items
      if (!live && selectedItems.size > 0) {
        val deathEvent = selectedItems(0).event
        deathLog.text = EventProcessor.eventsUpToDeath(deathEvent, events, EventType.DamageTypes ++ EventType.HealTypes).map {
          case ae: ActorEvent => Some("%d> %s".format(ae.time-events.head.time, ae.text))
          case _ => None
        }.flatten.mkString("\n")
      }
    }
  }

  def update(actor: Actor, events: List[LogEvent]) {
    this.events = events
    deathsList.listData = EventProcessor.actorDeaths(actor, events) map { e => ListEntry(e, e.time-events.head.time, e.text) }
    deathsList.selectIndices(0)
    pack()
  }

  case class ListEntry(event: ActorEvent, time: Long, text: String) {
    override def toString = "%d> %s".format(time, text)
  }
}
