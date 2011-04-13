package net.doxxx.riftcombatparser

import EventType._
import collection.mutable.HashMap
import collection.immutable.List._

object EventProcessor {
  def summary(events: List[LogEvent]): Map[String, Summary] = {
    val results = new HashMap[String, Summary] {
      override def default(key: String) = Summary()
    }
    for (e <- events) e match {
      case ae: ActorEvent if (DamageTypes.contains(ae.eventType)) => {
        results(ae.actor) = results(ae.actor).addDamageOut(ae.amount)
        results(ae.target) = results(ae.target).addDamageIn(ae.amount)
      }
      case ae: ActorEvent if (HealTypes.contains(ae.eventType)) => {
        results(ae.actor) = results(ae.actor).addHealingOut(ae.amount)
        results(ae.target) = results(ae.target).addHealingIn(ae.amount)
      }
      case ae: ActorEvent if (ae.eventType == Died) => {
        results(ae.actor) = results(ae.actor).addDeath()
      }
      case ae: ActorEvent if (ae.eventType == Slain) => {
        results(ae.target) = results(ae.target).addDeath()
      }
      case _ =>
    }
    results.toMap
  }

  def actors(events: List[LogEvent]): List[String] = {
    val activity = new HashMap[String, Int] {
      override def default(key: String) = 0
    }
    for (e <- events) e match {
      case ae: ActorEvent => activity(ae.actor) = activity(ae.actor) + 1
      case _ =>
    }
    def cmp(a1:Pair[String,Int], a2:Pair[String,Int]) = {
      a1._2 > a2._2
    }
    for ((k,v) <- activity.toList.sortWith(cmp)) yield k
  }

  def stripPreCombat(events: List[LogEvent]): List[LogEvent] = {
    events match {
      case Nil => Nil
      case _ => {
        val (start, rest) = events span {
          case CombatToggleEvent(time, state) => false
          case _ => true
        }
        rest match {
          case Nil => Nil
          case CombatToggleEvent(time, state) :: _ => {
            if (state) {
              rest
            }
            else {
              CombatToggleEvent(start.head.time, true) :: start ::: rest
            }
          }
          case _ => throw new IllegalStateException
        }
      }
    }
  }

  def splitFights(events: List[LogEvent]): List[List[LogEvent]] = {
    events match {
      case Nil => Nil
      case CombatToggleEvent(_, true) :: tail => {
        val (fight, rest) = tail span {
          case CombatToggleEvent(_, false) => false
          case _ => true
        }
        fight :: (rest match {
          case Nil => Nil
          case CombatToggleEvent(_, _) :: restTail => splitFights(restTail)
          case _ => throw new IllegalStateException
        })
      }
      case _ => {
        splitFights(stripPreCombat(events))
      }
    }
  }
}

case class Summary(damageIn: Int = 0, damageOut: Int = 0, healingIn: Int = 0, healingOut: Int = 0, deaths: Int = 0) {
  def addDamageIn(amount: Int) = copy(damageIn = damageIn + amount)
  def addDamageOut(amount: Int) = copy(damageOut = damageOut + amount)
  def addHealingIn(amount: Int) = copy(healingIn = healingIn + amount)
  def addHealingOut(amount: Int) = copy(healingOut = healingOut + amount)
  def addDeath() = copy(deaths = deaths + 1)
}
