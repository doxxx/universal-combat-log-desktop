package net.doxxx.riftcombatparser

sealed abstract class LogEvent(time: Long)

case class CombatToggleEvent(time: Long, state: Boolean) extends LogEvent(time)

case class ActorEvent(time: Long, eventType: EventType.Value, actor: String, target: String, spell: String,
                      spellId: Long, amount: Int, text: String) extends LogEvent(time)
