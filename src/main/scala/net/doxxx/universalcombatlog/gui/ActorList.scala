package net.doxxx.universalcombatlog.gui

import net.doxxx.universalcombatlog.parser.Entity
import scala.swing._
import scala.swing.event._

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
      text = "Entity Filter"
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

  var updating = false

  reactions += {
    case ButtonClicked(`top5Button`) => {
      listView.selectIndices(0, 1, 2, 3, 4)
    }
    case ButtonClicked(`resetButton`) => {
      listView.selectIndices()
    }
    case ListSelectionChanged(`listView`, range, isChanging) => {
      if (!isChanging && !updating) {
        publish(ActorFilterChanged(listView.selection.items.toSet))
      }
    }
  }

  def update(actors: Seq[Entity]) {
    updating = true
    val oldActors: Set[String] = listView.selection.items.toSet
    listView.listData = actors.map(_.name)
    selectActors(oldActors.toSet)
    updating = false
    publish(ActorFilterChanged(listView.selection.items.toSet))
  }

  def selectActors(names: Set[String]) {
    val indices = listView.listData.zipWithIndex.filter {
      case (name, index) => names.contains(name)
    } map {
      case (name, index) => index
    }
    listView.selectIndices(indices: _*)
  }

  def selectedActors: Set[String] = listView.selection.items.toSet
}

case class ActorFilterChanged(actors: Set[String]) extends Event
