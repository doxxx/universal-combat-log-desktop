package net.doxxx.riftcombatparser

object BreakdownType extends Enumeration {
  val None = Value
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

  def nameFor(breakdownType: Value): String = {
    breakdownType match {
      case OutgoingDamageBySpell => "Outgoing Damage By Spell"
      case OutgoingDamageByTarget => "Outgoing Damage By Target"
      case OutgoingHealingBySpell => "Outgoing Healing By Spell"
      case OutgoingHealingByTarget => "Outgoing Healing By Target"
      case IncomingDamageBySpell => "Incoming Damage By Spell"
      case IncomingDamageByActor => "Incoming Damage By Actor"
      case IncomingHealingBySpell => "Incoming Healing By Spell"
      case IncomingHealingByActor => "Incoming Healing By Actor"
      case _ => throw new IllegalArgumentException(breakdownType.toString)
    }
  }
}
