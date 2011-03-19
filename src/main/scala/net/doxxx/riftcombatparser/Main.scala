package net.doxxx.riftcombatparser

import io.Source
import collection.immutable.TreeMap

object Main {
  def main(args: Array[String]) {
    for (arg <- args) {
      parse(arg)
    }
  }

  def mapEventTypes(events: List[Event]) {
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

  def summary(events: List[Event]) {
    for ((name, summary) <- EventProcessor.summary(events)) {
      println("%s\t%d".format(name, summary))
    }
  }

  def print(events: List[Event]) {
    for (event <- events) {
      println(event)
    }
  }

  def parse(filename: String) {
    val events = new Parser(Source.fromFile(filename)).parse()
//    print(events)
//    mapEventTypes(events)
    summary(events)
  }
}
