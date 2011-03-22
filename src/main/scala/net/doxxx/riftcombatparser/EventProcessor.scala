package net.doxxx.riftcombatparser

import EventType._
import collection.mutable.{HashSet, HashMap}

object EventProcessor {
  def isDamageEvent(eventType: EventType.Value): Boolean = {
    eventType == DirectDamage || eventType == DamageOverTime || eventType == DamageOverTime
  }

  def isHealEvent(eventType: EventType.Value): Boolean = {
    eventType == Heal || eventType == CritHeal
  }

  def summary(events: List[Event]) = {
    val results = new HashMap[String, Summary] {
      override def default(key: String) = Summary()
    }
    for (e <- events) e match {
      case ae: ActorEvent if (isDamageEvent(ae.eventType)) => {
        results(ae.actor) = results(ae.actor).addDamageOut(ae.amount)
        results(ae.target) = results(ae.target).addDamageIn(ae.amount)

      }
      case ae: ActorEvent if (isHealEvent(ae.eventType)) => {
        results(ae.actor) = results(ae.actor).addHealingOut(ae.amount)
        results(ae.target) = results(ae.target).addHealingIn(ae.amount)

      }
      case _ =>
    }
    results.toMap
  }

  def actors(events: List[Event]) = {
    val names = new HashSet[String]
    for (e <- events) e match {
      case ae: ActorEvent => names += ae.actor
      case _ =>
    }
    names.toSet
  }
}

case class Summary(damageIn: Int = 0, damageOut: Int = 0, healingIn: Int = 0, healingOut: Int = 0) {
  def addDamageIn(amount: Int) = copy(damageIn = damageIn + amount)
  def addDamageOut(amount: Int) = copy(damageOut = damageOut + amount)
  def addHealingIn(amount: Int) = copy(healingIn = healingIn + amount)
  def addHealingOut(amount: Int) = copy(healingOut = healingOut + amount)
}
