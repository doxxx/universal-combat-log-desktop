package net.doxxx.riftcombatparser

import net.doxxx.riftcombatparser.CombatLogParser.{NullActorID, ActorID}

sealed abstract class LogEvent(val time: Long) {
  def copy(newTime: Long): LogEvent
}

case class CombatToggleEvent(override val time: Long, state: Boolean) extends LogEvent(time) {
  def copy(newTime: Long) = CombatToggleEvent(newTime, state)
}

case class ActorEvent(override val time: Long, eventType: EventType.Value,
                      actor: Actor, target: Actor, spell: String, spellId: Long, amount: Int, text: String)
                      extends LogEvent(time) {
  def copy(newTime: Long) = ActorEvent(newTime, eventType, actor, target, spell, spellId, amount, text)
}

sealed abstract class Actor(val id: ActorID) {
  def name: String
  def name_=(newName: String)
}

case object Nobody extends Actor(NullActorID) {
  def name = "Mr Nobody!"
  override def name_=(newName: String) {
    throw new RuntimeException("Cannot rename Mr Nobody!")
  }
}

case class Player(override val id: ActorID, var name: String) extends Actor(id)

case class PlayerPet(override val id: ActorID, var _name: String, owner: Player) extends Actor(id) {
  def name = _name + " (" + owner.name + ")"

  override def name_=(newName: String) {
    if (_name != newName) {
      println("Actor %s has changed name: %s -> %s".format(id, _name, newName))
      _name = newName
    }
  }
}

case class NonPlayer(override val id: ActorID, var name: String) extends Actor(id)

