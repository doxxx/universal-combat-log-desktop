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

  val DamageTypes = Set(OutgoingDamageByTarget, OutgoingDamageBySpell, IncomingDamageByActor, IncomingDamageBySpell)
  val HealingTypes = Set(OutgoingHealingBySpell, OutgoingHealingByTarget, IncomingHealingByActor, IncomingHealingBySpell)
  val BySpellTypes = Set(OutgoingDamageBySpell, OutgoingHealingBySpell, IncomingDamageBySpell, IncomingHealingBySpell)
  val ByTargetTypes = Set(OutgoingDamageByTarget, OutgoingHealingByTarget)
  val ByActorTypes = Set(IncomingDamageByActor, IncomingHealingByActor)
  val IncomingTypes = Set(IncomingDamageByActor, IncomingDamageBySpell, IncomingHealingByActor, IncomingHealingBySpell)
  val OutgoingTypes = Set(OutgoingDamageBySpell, OutgoingDamageByTarget, OutgoingHealingBySpell, OutgoingHealingByTarget)
}
