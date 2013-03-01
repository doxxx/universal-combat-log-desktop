package net.doxxx.universalcombatlog

import java.io.File
import net.doxxx.universalcombatlog.Utils._
import net.doxxx.universalcombatlog.parser._
import scala.collection.immutable.TreeMap

object InspectLog {

  def mapEventTypes(events: List[LogEvent]) {
    var m = TreeMap[EventTypes.Value, String]()
    for (event <- events) {
      event match {
        case ce: CombatEvent => m += ce.eventType -> ce.text
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
        val logFile = new WoWParser().parse(new File(arg))
        log("%d events loaded.", logFile.events.length)
        val fights = timeit("fight-split") {
          logFile.fights
        }
        log("%d fights found.", fights.length)
        log(fights.map(_.duration / 1000).mkString(" "))
        System.out.flush()
        readLine("Press enter to continue...")
      }
    }
  }

}
