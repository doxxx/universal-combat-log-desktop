package net.doxxx.universalcombatlog.parser

import net.doxxx.universalcombatlog.spells.Spell

sealed abstract class LogEvent {
  def time: Long
  def copy(newTime: Long): LogEvent
}

case class CombatToggleEvent(time: Long, inCombat: Boolean) extends LogEvent {
  def copy(newTime: Long) = CombatToggleEvent(newTime, inCombat)
}

case class CombatEvent(time: Long, eventType: EventTypes.Value, actor: Entity, target: Entity, spell: Spell,
                       spellSchool: String, amount: Int, overAmount: Int, text: String) extends LogEvent
{
  def copy(newTime: Long) = CombatEvent(newTime, eventType, actor, target, spell, spellSchool, amount, overAmount, text)
}

