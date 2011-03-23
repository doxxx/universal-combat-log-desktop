package net.doxxx.riftcombatparser

import swing.{ListView, ScrollPane}
import swing.event.{Event, ListSelectionChanged}

class ActorList extends ScrollPane {
  val listView = new ListView[String]
  contents = listView

  listenTo(listView.selection)

  reactions += {
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
