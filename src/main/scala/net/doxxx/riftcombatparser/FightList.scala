package net.doxxx.riftcombatparser

import swing._
import event._

class FightList extends BorderPanel {
  layoutManager.setHgap(5)
  layoutManager.setVgap(5)

  val header = new BoxPanel(Orientation.Horizontal) {
    contents += new Label {
      text = "Fights"
    }
  }
  val listView = new ListView[Fight]
  val scrollPane = new ScrollPane {
    contents = listView
  }

  layout(header) = BorderPanel.Position.North
  layout(scrollPane) = BorderPanel.Position.Center

  listenTo(listView.selection)

  var updating = false

  reactions += {
    case ListSelectionChanged(`listView`, range, isChanging) => {
      if (!isChanging && !updating) {
        publish(SelectedFightsChanged(listView.selection.items.toList))
      }
    }
  }

  def update(events: List[LogEvent]) {
    updating = true
    val oldFightNames = listView.selection.items.map{ _.toString }.toSet
    val fights = EventProcessor.splitFights(events)
    val everything = Fights(fights, Some("Everything"))
    listView.listData = everything :: fights
    selectFightsByName(oldFightNames)
    updating = false
    publish(SelectedFightsChanged(listView.selection.items.toList))
  }

  def selectFightsByName(fightNames: Set[String]) {
    val indices = listView.listData.zipWithIndex.filter {
      case (fight, index) => fightNames.contains(fight.toString)
    } map {
      case (fight, index) => index
    }
    listView.selectIndices(indices: _*)
  }

  def selectedFights: List[Fight] = listView.selection.items.toList
}

case class SelectedFightsChanged(fights: List[Fight]) extends Event
