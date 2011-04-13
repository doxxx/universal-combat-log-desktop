package net.doxxx.riftcombatparser

sealed abstract class LogEvent(val time: Long)

case class CombatToggleEvent(override val time: Long, state: Boolean) extends LogEvent(time)

case class ActorEvent(override val time: Long, eventType: EventType.Value, actor: String, target: String, spell: String,
                      spellId: Long, amount: Int, text: String) extends LogEvent(time)
