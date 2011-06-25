package net.doxxx.riftcombatparser

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

sealed abstract class Actor {
  def name: String
  def name_=(s: String)
}
case object Nobody extends Actor {
  def name = "Mr Nobody!"
  def name_=(s: String) {
    throw new RuntimeException("Cannot rename Mr Nobody!")
  }
}
case class Player(var name: String) extends Actor
case class PlayerPet(var _name: String, owner: Player) extends Actor {
  def name = _name + " (" + owner.name + ")"
  def name_=(s: String) { _name = s }
}
case class NonPlayer(var name: String) extends Actor

