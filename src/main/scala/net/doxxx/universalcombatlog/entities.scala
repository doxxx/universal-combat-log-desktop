package net.doxxx.universalcombatlog

sealed abstract class ActorID {
  def id: Long
  def rel: Char
}
case object NullActorID extends ActorID {
  val id = 0L
  val rel = 'X'
}
case class NPC(id: Long, rel: Char) extends ActorID
case class PC(id: Long, rel: Char) extends ActorID

sealed abstract class Actor(val id: ActorID) {
  def name: String
  def name_=(newName: String)
  def grouped: Boolean = id.rel == 'C' || id.rel == 'G' || id.rel == 'R'
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

