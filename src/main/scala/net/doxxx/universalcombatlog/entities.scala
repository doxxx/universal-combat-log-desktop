package net.doxxx.universalcombatlog

sealed abstract class EntityID {
  def id: Long
  def rel: Char
}
case object NullEntityID extends EntityID {
  val id = 0L
  val rel = 'X'
}
case class NPC(id: Long, rel: Char) extends EntityID
case class PC(id: Long, rel: Char) extends EntityID

sealed abstract class Entity(val id: EntityID) {
  def name: String
  def name_=(newName: String)
  def grouped: Boolean = id.rel == 'C' || id.rel == 'G' || id.rel == 'R'
}

case object Nobody extends Entity(NullEntityID) {
  def name = "Mr Nobody!"
  override def name_=(newName: String) {
    throw new RuntimeException("Cannot rename Mr Nobody!")
  }
}

case class Player(override val id: EntityID, var name: String) extends Entity(id)

case class PlayerPet(override val id: EntityID, var _name: String, owner: Player) extends Entity(id) {
  def name = _name + " (" + owner.name + ")"

  override def name_=(newName: String) {
    if (_name != newName) {
      println("Entity %s has changed name: %s -> %s".format(id, _name, newName))
      _name = newName
    }
  }
}

case class NonPlayer(override val id: EntityID, var name: String) extends Entity(id)

