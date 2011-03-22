package net.doxxx.riftcombatparser

import swing.{ListView, ScrollPane}

class ActorList(names: List[String]) extends ScrollPane {
  val listView = new ListView[String]
  contents = listView
  update(names)
  
  def update(names: List[String]) {
    listView.listData = names
  }
}
