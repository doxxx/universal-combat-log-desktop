package net.doxxx.riftcombatparser

import swing._
import event._

class ActorList extends BorderPanel {
  layoutManager.setHgap(5)
  layoutManager.setVgap(5)
  
  val resetButton = new Button {
    text = "Reset"
  }
  val header = new BoxPanel(Orientation.Horizontal) {
    contents += new Label {
      text = "Actor Filter"
    }
    contents += Swing.HGlue
    contents += resetButton
  }
  val listView = new ListView[String]
  val scrollPane = new ScrollPane {
    contents = listView
  }

  layout(header) = BorderPanel.Position.North
  layout(scrollPane) = BorderPanel.Position.Center

  listenTo(resetButton)
  listenTo(listView.selection)

  reactions += {
    case ButtonClicked(`resetButton`) => {
      listView.selectIndices()
    }
    case ListSelectionChanged(`listView`, range, isChanging) => {
      if (!isChanging) {
        publish(SelectedActorsChanged(listView.selection.items.toList))
      }
    }
  }

  def update(names: List[String]) {
    listView.listData = names
  }
}

case class SelectedActorsChanged(actors: List[String]) extends Event
