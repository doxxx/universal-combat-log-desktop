package net.doxxx.riftcombatparser

import io.Source
import util.matching.Regex

object CombatLogParser {
  private val CombatToggleRE = new Regex("([0-9][0-9]:[0-9][0-9]:[0-9][0-9]) Combat (Begin|End)", "time", "toggle")
  private val DataRE =
    new Regex("([0-9]+) , T=.+ , T=.+ , T=.+ , T=.+ , (.*?) , (.*?) , (-?[0-9]*) , ([0-9]*) , (.*?)",
              "eventType", "actor", "target", "amount", "spellId", "spell")
  private val LineRE = new Regex("([0-9][0-9]:[0-9][0-9]:[0-9][0-9]): \\( (.+?) \\) (.+)", "time", "data", "text")
  private val OverhealRE = new Regex("\\(([0-9]+) overheal\\)", "amount")

  def parse(source: Source): List[LogEvent] = {
    try {
      Utils.timeit("logparse") { () =>
        source.getLines().toList.par.map(parseLine).toList.flatten
      }
    }
    finally {
      source.close()
    }
  }

  def extractOverheal(text: String): Int = {
    OverhealRE.findFirstMatchIn(text) match {
      case Some(m) => m.group("amount").toInt
      case None => 0
    }
  }

  private def parseLine(line: String): Option[LogEvent] = {
    line match {
      case CombatToggleRE(time, toggle) => Some(CombatToggleEvent(parseTime(time), parseCombatToggle(toggle)))
      case LineRE(time, data, text) => parseActorEvent(time, data, text)
      case _ => {
        println("Unrecognized combat log line: " + line)
        None
      }
    }
  }

  private def parseTime(s: String): Long = {
    val parts = s.split(':')
    parts(0).toInt * 60 * 60 + parts(1).toInt * 60 + parts(2).toInt
  }

  private def parseCombatToggle(toggle: String): Boolean = toggle match {
    case "Begin" => true
    case "End" => false
    case _ => throw new IllegalArgumentException("Unrecognized combat toggle: " + toggle)
  }

  private def parseActorEvent(time: String, data: String, text: String): Option[ActorEvent] = {
    data match {
      case DataRE(eventType, actor, target, amount, spellId, spell) => Some(ActorEvent(parseTime(time),
        EventType(eventType.toInt), actor, target, spell, spellId.toLong, amount.toInt, text))
      case _ => {
        println("Unrecognized data string: " + data)
        None
      }
    }
  }
}
