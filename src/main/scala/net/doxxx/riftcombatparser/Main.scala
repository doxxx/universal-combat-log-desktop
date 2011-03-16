package net.doxxx.riftcombatparser

import io.Source
import collection.mutable.Map
import collection.immutable.TreeMap

object Main {
  def main(args: Array[String]) {
    for (arg <- args) {
      parse(arg)
    }
  }

  def filter(events: scala.List[Event]) {
    var m = TreeMap[EventType.Value, String]()
    for (event <- events) {
      event match {
        case ae: ActorEvent => m += ae.eventType -> ae.text
        case _ =>
      }
    }
    for ((id, text) <- m) {
      println("%s\t%s".format(id.toString, text))
    }
  }

  def print(events: scala.List[Event]) {
    for (event <- events) {
      println(event)
    }
  }

  def parse(filename: String) {
    val events = new Parser(Source.fromFile(filename)).parse()
    print(events)
//    filter(events)
  }
}
