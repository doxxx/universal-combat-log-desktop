package net.doxxx.riftcombatparser

object BreakdownType extends Enumeration {
  val OutgoingDamageBySpell = Value
  val OutgoingDamageByTarget = Value
  val OutgoingHealingBySpell = Value
  val OutgoingHealingByTarget = Value
  val IncomingDamageBySpell = Value
  val IncomingDamageByActor = Value
  val IncomingHealingBySpell = Value
  val IncomingHealingByActor = Value
}
