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

  reactions += {
    case ListSelectionChanged(`listView`, range, isChanging) => {
      if (!isChanging) {
        publish(SelectedFightsChanged(listView.selection.items.toList))
      }
    }
  }

  def update(events: List[LogEvent]) {
    val oldFights: Seq[Fight] = listView.selection.items
    val everything = Fight(events, Some("Everything"))
    listView.listData = everything :: EventProcessor.splitFights(events)
    selectFights(oldFights.toSet)
  }

  def selectFights(fights: Set[Fight]) {
    val indices = listView.listData.zipWithIndex.filter {
      case (fight, index) => fights.contains(fight)
    } map {
      case (fight, index) => index
    }
    listView.selectIndices(indices: _*)
  }

  def selectedFights: List[Fight] = listView.selection.items.toList
}

case class SelectedFightsChanged(fights: List[Fight]) extends Event