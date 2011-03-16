package net.doxxx.riftcombatparser

import io.Source
import util.matching.Regex

class Parser(source: Source) {
  private val CombatToggleRE = new Regex("([0-9][0-9]:[0-9][0-9]:[0-9][0-9]) Combat (Begin|End)", "time", "toggle")
  private val DataRE = new Regex("\\( ([0-9]+) , T=.+ , T=.+ , T=.+ , T=.+ , (.*?) , (.*?) , (-?[0-9]*) , ([0-9]*) , (.*?) \\)",
                                 "eventType", "actor", "target", "amount", "spellId", "spell")
  private val LineRE = new Regex("([0-9][0-9]:[0-9][0-9]:[0-9][0-9]): (\\(.+?\\)) (.+)", "time", "data", "text")

  def parse(): List[Event] = {
    source.getLines().map(parseLine).toList.flatten
  }

  def parseLine(line: String): Option[Event] = {
    CombatToggleRE.findPrefixMatchOf(line) match {
      case Some(m) => Some(CombatToggleEvent(parseTime(m.group("time")), parseCombatToggle(m.group("toggle"))))
      case None => LineRE.findPrefixMatchOf(line) match {
        case Some(m) => Some(parseActorEvent(m.group("time"), m.group("data"), m.group("text")))
        case None => throw new IllegalArgumentException("Unrecognized combat log line: " + line)
      }
    }
  }

  def parseTime(s: String): Long = {
    val parts = s.split(':')
    parts(0).toInt * 60 * 60 + parts(1).toInt * 60 + parts(2).toInt
  }

  def parseCombatToggle(toggle: String): Boolean = {
    toggle match {
      case "Begin" => true
      case "End" => false
      case _ => throw new IllegalArgumentException("Unrecognized combat toggle: " + toggle)
    }
  }

  def parseActorEvent(time: String, data: String, text: String): ActorEvent = {
    DataRE.findPrefixMatchOf(data) match {
      case Some(m) => ActorEvent(parseTime(time), EventType(m.group("eventType").toInt), m.group("actor"), m.group("target"),
                                 m.group("spell"), m.group("spellId").toLong, m.group("amount").toInt, text)
      case None => throw new IllegalArgumentException("Unrecognized data string: " + data)
    }
  }
}

sealed abstract class Event(time: Long)

case class CombatToggleEvent(time: Long, state: Boolean) extends Event(time)

case class ActorEvent(time: Long, eventType: EventType.Value, actor: String, target: String, spell: String, spellId: Long, amount: Int, text: String) extends Event(time)

//case class Heal(time: Long, actor: String, target: String, spell: String, amount: Int) extends ActorEvent(time, actor, target, spell)
//case class Damage(time: Long, actor: String, target: String, spell: String, amount: Int) extends ActorEvent(time, actor, target, spell)
