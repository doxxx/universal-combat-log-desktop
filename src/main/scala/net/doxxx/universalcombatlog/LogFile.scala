package net.doxxx.universalcombatlog

import net.doxxx.universalcombatlog.parser.{PlayerPet, Player, Entity, LogEvent}
import net.doxxx.universalcombatlog.spells.Spell

class LogFile(val name: String, val events: List[LogEvent], val entities: Set[Entity], val spells: Set[Spell]) {
  lazy val playersAndPets: Set[Entity] = entities.filter {
    case _: Player => true
    case _: PlayerPet => true
    case _ => false
  }

  lazy val fights: List[Fight] = EventProcessor.splitFights(events).filter(_.duration > 5000)

  def normalizeTimes(startTime: Long = 0): LogFile =
    new LogFile(name, EventProcessor.normalizeTimes(events, startTime), entities, spells)
}
