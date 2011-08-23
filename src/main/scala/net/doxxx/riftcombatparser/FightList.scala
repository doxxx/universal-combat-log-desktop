package net.doxxx.riftcombatparser

import swing._
import event._

class FightList extends BorderPanel {
  layoutManager.setHgap(5)
  layoutManager.setVgap(5)

  val allButton = new Button {
    text = "All"
  }

  val header = new BoxPanel(Orientation.Horizontal) {
    contents += new Label {
      text = "Fights"
    }
    contents += Swing.HGlue
    contents += allButton
  }
  val listView = new ListView[Fight]
  val scrollPane = new ScrollPane {
    contents = listView
  }

  layout(header) = BorderPanel.Position.North
  layout(scrollPane) = BorderPanel.Position.Center

  listenTo(allButton)
  listenTo(listView.selection)

  var updating = false

  reactions += {
    case ButtonClicked(`allButton`) => {
      listView.peer.getSelectionModel.clearSelection()
      listView.peer.getSelectionModel.addSelectionInterval(0, listView.listData.size-1)
    }
    case ListSelectionChanged(`listView`, range, isChanging) => {
      if (!isChanging && !updating) {
        fireSelectedFightsChanged()
      }
    }
  }

  def update(fights: List[Fight]) {
    updating = true
    val oldFights = listView.selection.items.map{ _.startTime }.toSet
    listView.listData = fights
    selectFightsByStartTime(oldFights)
    updating = false
    fireSelectedFightsChanged()
  }

  def selectFightsByStartTime(fightNames: Set[Long]) {
    val indices = listView.listData.zipWithIndex.filter {
      case (fight, index) => fightNames.contains(fight.startTime)
    } map {
      case (fight, index) => index
    }
    listView.selectIndices(indices: _*)
  }

  def selectedFights: List[Fight] = listView.selection.items.toList

  def fireSelectedFightsChanged() {
    publish(SelectedFightsChanged(listView.selection.items.toList))
  }

}

case class SelectedFightsChanged(fights: List[Fight]) extends Event
