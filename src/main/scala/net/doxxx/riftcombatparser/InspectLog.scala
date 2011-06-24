package net.doxxx.riftcombatparser

import io.Source
import collection.immutable.TreeMap
import collection.mutable.HashMap

object InspectLog {

  def main(args: Array[String]) {
    for (arg <- args) {
      val events = CombatLogParser.parse(Source.fromFile(arg))
      //mapEventTypes(events)
      mapEntityRValuesToIDs(events)
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

  def mapEntityRValuesToIDs(events: List[LogEvent]) {
    val m = new HashMap[Char, Set[String]]() {
      override def default(key: Char) = Set.empty
    }
    for (event <- events) {
      event match {
        case ae: ActorEvent => {
          ae.actorInfo match {
            case PC(r, id) => m += r -> (m(r) + ("P" + id.toString))
            case NPC(r, id) => m += r -> (m(r) + ("N" + id.toString))
            case _ => // nothing
          }
        }
        case _ => // do nothing
      }
    }
    for ((r, ids) <- m) {
      println("%c => %s".format(r, ids.mkString(", ")))
    }
  }
}
