package net.doxxx.universalcombatlog

package object spells {
  case class Spell(id: Long, name: String)
  val NullSpell = Spell(0, "")
}
