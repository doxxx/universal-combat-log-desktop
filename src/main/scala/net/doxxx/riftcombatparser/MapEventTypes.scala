package net.doxxx.riftcombatparser

import io.Source
import collection.immutable.TreeMap

object MapEventTypes {
  def main(args: Array[String]) {
    for (arg <- args) {
      val events = CombatLogParser.parse(Source.fromFile(arg))
      mapEventTypes(events)
    }
  }

  def mapEventTypes(events: List[LogEvent]) {
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
}
