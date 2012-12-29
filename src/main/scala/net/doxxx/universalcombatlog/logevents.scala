package net.doxxx.universalcombatlog

sealed abstract class LogEvent {
  def time: Long
  def copy(newTime: Long): LogEvent
}

case class CombatToggleEvent(time: Long, inCombat: Boolean) extends LogEvent {
  def copy(newTime: Long) = CombatToggleEvent(newTime, inCombat)
}

case class ActorEvent(time: Long, eventType: EventTypes.Value,
                      actor: Entity, target: Entity, spell: String, spellId: Long, amount: Int, text: String)
                      extends LogEvent {
  def copy(newTime: Long) = ActorEvent(newTime, eventType, actor, target, spell, spellId, amount, text)
}

