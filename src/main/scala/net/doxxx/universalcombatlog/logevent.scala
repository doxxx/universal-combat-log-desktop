package net.doxxx.universalcombatlog

sealed abstract class LogEvent(val time: Long) {
  def copy(newTime: Long): LogEvent
}

case class CombatToggleEvent(override val time: Long, inCombat: Boolean) extends LogEvent(time) {
  def copy(newTime: Long) = CombatToggleEvent(newTime, inCombat)
}

case class ActorEvent(override val time: Long, eventType: EventType.Value,
                      actor: Entity, target: Entity, spell: String, spellId: Long, amount: Int, text: String)
                      extends LogEvent(time) {
  def copy(newTime: Long) = ActorEvent(newTime, eventType, actor, target, spell, spellId, amount, text)
}

