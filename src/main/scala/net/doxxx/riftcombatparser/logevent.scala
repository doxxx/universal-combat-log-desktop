package net.doxxx.riftcombatparser

sealed abstract class LogEvent(val time: Long) {
  def copy(newTime: Long): LogEvent
}

case class CombatToggleEvent(override val time: Long, state: Boolean) extends LogEvent(time) {
  def copy(newTime: Long) = CombatToggleEvent(newTime, state)
}

case class ActorEvent(override val time: Long, actorInfo: Entity, targetInfo: Entity,
                      actorOwnerInfo: Entity, targetOwnerInfo: Entity, eventType: EventType.Value,
                      actor: String, target: String, spell: String, spellId: Long, amount: Int, text: String)
                      extends LogEvent(time) {
  def copy(newTime: Long) = ActorEvent(newTime, actorInfo, targetInfo, actorOwnerInfo, targetOwnerInfo, eventType,
    actor, target, spell, spellId, amount, text)
}

sealed abstract class Entity(val rel: Char, val id: Long)

case object Nobody extends Entity('X', 0)
case class NPC(override val rel: Char, override val id: Long) extends Entity(rel, id)
case class PC(override val rel: Char, override val id: Long) extends Entity(rel, id)
