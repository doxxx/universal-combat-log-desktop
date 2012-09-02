package net.doxxx.riftcombatparser

import collection.immutable.TreeMap
import net.doxxx.riftcombatparser.Utils._
import java.io.File

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
      while (true) {
        val events = EventProcessor.normalizeTimes(CombatLogParser.parse(new File(arg)))
        log("%d events loaded.", events.length)
        val fights = timeit("fight-split") {
          EventProcessor.splitFights(events)
        }
        log("%d fights found.", fights.length)
        log(fights.map(_.duration / 1000).mkString(" "))
        System.out.flush()
        readLine("Press enter to continue...")
      }
    }
  }

}
