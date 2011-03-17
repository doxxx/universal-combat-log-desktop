package net.doxxx.riftcombatparser

import collection.mutable.HashMap


object EventProcessor {
  def isDamageEvent(e: ActorEvent): Boolean = {
    e.eventType == EventType.DirectDamage || e.eventType == EventType.DamageOverTime || e.eventType == EventType.DamageOverTime
  }

  def damageSummary(events: List[Event]) = {
    val summary = new HashMap[String, Int] {
      override def default(key: String) = 0
    }
    events foreach {
      case e: ActorEvent if (isDamageEvent(e)) => {
        summary(e.actor) = summary(e.actor) + e.amount
      }
      case _ =>
    }
    summary.toMap
  }
}
