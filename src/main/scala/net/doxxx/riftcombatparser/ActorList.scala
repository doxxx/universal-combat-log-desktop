package net.doxxx.riftcombatparser

import swing._
import event._

class ActorList extends BorderPanel {
  layoutManager.setHgap(5)
  layoutManager.setVgap(5)

  val top5Button = new Button {
    text = "Top 5"
  }
  val resetButton = new Button {
    text = "Reset"
  }
  val header = new BoxPanel(Orientation.Horizontal) {
    contents += new Label {
      text = "Actor Filter"
    }
    contents += Swing.HGlue
    contents += top5Button
    contents += Swing.HStrut(5)
    contents += resetButton
  }
  val listView = new ListView[String]
  val scrollPane = new ScrollPane {
    contents = listView
  }

  layout(header) = BorderPanel.Position.North
  layout(scrollPane) = BorderPanel.Position.Center

  listenTo(top5Button)
  listenTo(resetButton)
  listenTo(listView.selection)

  reactions += {
    case ButtonClicked(`top5Button`) => {
      listView.selectIndices(0, 1, 2, 3, 4)
    }
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

  def selectActors(names: Set[String]) {
    val indices = listView.listData.zipWithIndex.filter {
      case (name, index) => names.contains(name)
    } map {
      case (name, index) => index
    }
    listView.selectIndices(indices: _*)
  }
}

case class SelectedActorsChanged(actors: List[String]) extends Event
