package net.doxxx.riftcombatparser

import collection.mutable.HashMap
import EventType._

object EventProcessor {
  def isDamageEvent(e: ActorEvent): Boolean = {
    e.eventType == EventType.DirectDamage || e.eventType == EventType.DamageOverTime || e.eventType == EventType.DamageOverTime
  }

  private def addDamage(results: HashMap[String, Summary], actor: String, target: String, amount: Int) {
    if (!results.contains(actor)) {
      results(actor) = new Summary(0, 0, 0, 0)
    }
    results(actor).damageOut += amount

    if (!results.contains(target)) {
      results(target) = new Summary(0, 0, 0, 0)
    }
    results(target).damageIn += amount
  }

  private def addHealing(results: HashMap[String, Summary], actor: String, target: String, amount: Int) {
    if (!results.contains(actor)) {
      results(actor) = new Summary(0, 0, 0, 0)
    }
    results(actor).healingOut += amount

    if (!results.contains(target)) {
      results(target) = new Summary(0, 0, 0, 0)
    }
    results(target).healingIn += amount
  }

  def summary(events: List[Event]) = {
    val results = new HashMap[String, Summary]
    for (e <- events) e match {
      case ae: ActorEvent => ae.eventType match {
        case DamageOverTime => addDamage(results, ae.actor, ae.target, ae.amount)
        case DirectDamage => addDamage(results, ae.actor, ae.target, ae.amount)
        case EnvDamage => addDamage(results, ae.actor, ae.target, ae.amount)
        case CritDamage => addDamage(results, ae.actor, ae.target, ae.amount)
        case Heal => addHealing(results, ae.actor, ae.target, ae.amount)
        case CritHeal => addHealing(results, ae.actor, ae.target, ae.amount)
        case _ =>
      }
      case _ =>
    }
    results.toMap
  }

}

class Summary(var damageIn: Int,
              var damageOut: Int,
              var healingIn: Int,
              var healingOut: Int)
