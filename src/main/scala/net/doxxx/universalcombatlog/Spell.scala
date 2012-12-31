package net.doxxx.universalcombatlog

case class Spell(id: Long, name: String)

case object NullSpell extends Spell(0, "")
