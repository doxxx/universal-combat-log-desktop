package net.doxxx.riftcombatparser

import io.Source
import collection.immutable.TreeMap
import net.doxxx.riftcombatparser.Utils._

object InspectLog {

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

  def main(args: Array[String]) {
    for (arg <- args) {
      val events = EventProcessor.normalizeTimes(CombatLogParser.parse(Source.fromFile(arg)))
      log("%d events loaded.", events.length)
      while (true) {
        System.out.flush()
        readLine()
        val fights = timeit("fight-split") {
          () => EventProcessor.splitFights(events)
        }
        log("%d fights found.", fights.length)
        log(fights.map(_.duration).mkString(" "))
      }
    }
  }

}
