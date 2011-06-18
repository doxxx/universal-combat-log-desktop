package net.doxxx.riftcombatparser

import EventType._
import collection.immutable.List._
import collection.mutable.HashMap

object EventProcessor {
  def summary(fight: Fight): Map[String, Summary] = {
    val results = new HashMap[String, Summary] {
      override def default(key: String) = Summary()
    }
    Utils.timeit("summary") { () =>
      for (e <- fight.events) e match {
        case ae: ActorEvent if (DamageTypes.contains(ae.eventType)) => {
          results(ae.actor) = results(ae.actor).addDamageOut(ae.amount)
          results(ae.target) = results(ae.target).addDamageIn(ae.amount)
        }
        case ae: ActorEvent if (HealTypes.contains(ae.eventType)) => {
          results(ae.actor) = results(ae.actor).addHealingOut(ae.amount)
          results(ae.target) = results(ae.target).addHealingIn(ae.amount)
          results(ae.target) = results(ae.target).addOverhealing(CombatLogParser.extractOverheal(ae.text))
        }
        case ae: ActorEvent if (ae.eventType == Died) => {
          results(ae.actor) = results(ae.actor).addDeath()
        }
        case ae: ActorEvent if (ae.eventType == Slain) => {
          results(ae.target) = results(ae.target).addDeath()
        }
        case _ =>
      }
      for (actor <- results.keys) {
        results(actor) = results(actor).calculatePerSecond(fight.duration)
      }
    }
    results.toMap
  }

  def actorsSortedByActivity(events: List[LogEvent]): List[String] = {
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

  def splitFights(events: List[LogEvent]): List[SingleFight] = {
    events match {
      case Nil => Nil
      case (start @ CombatToggleEvent(_, true)) :: tail => {
        val (fight, rest) = tail span {
          case CombatToggleEvent(_, false) => false
          case _ => true
        }
        // fight might be empty, so prepend the combat start event so it can calc start/end time
        SingleFight(start :: fight) :: (rest match {
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

  private val dayTime = 24*60*60
  def normalizeTimes(events: List[LogEvent]): List[LogEvent] = {
    val startTime = events.head.time
    for (e <- events) yield {
      val relTime = e.time - startTime
      if (relTime < 0) {
        e.copy(relTime + dayTime)
      }
      else {
        e.copy(relTime)
      }
    }
  }

  def spellBreakdown(events: List[LogEvent]): Map[String, SpellBreakdown] = {
    val results = new HashMap[String, SpellBreakdown] {
      override def default(key: String) = SpellBreakdown()
    }
    for (e <- events) e match {
      case ae: ActorEvent => {
        if (DamageTypes.contains(ae.eventType)) {
          if (ae.eventType == CritDamage) {
            results(ae.spell) = results(ae.spell).addCrit()
            results(ae.spell) = results(ae.spell).addDamage(ae.amount)
          }
          else if (MissTypes.contains(ae.eventType)) {
            results(ae.spell) = results(ae.spell).addMiss()
          }
          else {
            results(ae.spell) = results(ae.spell).addDamage(ae.amount)
            results(ae.spell) = results(ae.spell).addHit()
          }
        }
        else if (HealTypes.contains(ae.eventType)) {
          results(ae.spell) = results(ae.spell).addHealing(ae.amount)
          if (ae.eventType == CritHeal) {
            results(ae.spell) = results(ae.spell).addCrit()
          }
          else {
            results(ae.spell) = results(ae.spell).addHit()
          }
        }
      }
      case _ =>
    }
    results.toMap
  }

  def filterByActors(events: List[LogEvent], actors: Set[String]) = {
      if (actors.isEmpty)
        events
      else
        events filter {
          case ActorEvent(_, _, actor, _, _, _, _, _) => actors.contains(actor)
          case _ => true
        }
  }

  def dpsSummary(data: Map[String, Summary]) = data.map {case (name, summary) => name -> summary.dpsOut}

  def dpsSorted(data: Map[String, Summary]) = dpsSummary(data).toList.sortBy {case (name, value) => value}.reverse

  def hpsSummary(data: Map[String, Summary]) = data.map {case (name, summary) => name -> summary.hpsOut}

  def hpsSorted(data: Map[String, Summary]) = hpsSummary(data).toList.sortBy {case (name, value) => value}.reverse

  def raidDPS(data: Map[String, Summary]) = dpsSummary(data).map {case (name, value) => value}.sum

  def raidHPS(data: Map[String, Summary]) = hpsSummary(data).map {case (name, value) => value}.sum

  def dpsSummaryForClipboard(data: Map[String, Summary]): String = {
    val dps = dpsSorted(data).take(10).filter {case (name, value) => value > 0}
    "DPS: Raid:%d - %s".format(
      raidDPS(data),
      (dps.map {case (name, value) => "%.4s:%d".format(name, value)}).mkString(", ")
    )
  }

  def hpsSummaryForClipboard(data: Map[String, Summary]): String = {
    val hps = hpsSorted(data).take(10).filter {case (name, value) => value > 0}
    "HPS: Raid:%d - %s".format(
      raidHPS(data),
      (hps.map {case (name, value) => "%.4s:%d".format(name, value)}).mkString(", ")
    )
  }

  def filterSummaryByActors(summary: Map[String, Summary], actors: Set[String]): Map[String, Summary] = {
    if (actors.isEmpty)
      summary
    else
      summary filter { case (actor, sum) => actors.contains(actor) }
  }
}

case class Summary(damageIn: Int = 0, dpsIn: Int = 0,
                   damageOut: Int = 0, dpsOut: Int = 0,
                   healingIn: Int = 0, hpsIn: Int = 0,
                   healingOut: Int = 0, hpsOut: Int = 0,
                   overhealing: Int = 0,
                   deaths: Int = 0) {
  def addDamageIn(amount: Int) = copy(damageIn = damageIn + amount)
  def addDamageOut(amount: Int) = copy(damageOut = damageOut + amount)
  def addHealingIn(amount: Int) = copy(healingIn = healingIn + amount)
  def addHealingOut(amount: Int) = copy(healingOut = healingOut + amount)
  def addOverhealing(amount: Int) = copy(overhealing = overhealing + amount)
  def addDeath() = copy(deaths = deaths + 1)
  def calculatePerSecond(duration: Int) = copy(dpsIn = damageIn / duration, dpsOut = damageOut / duration,
    hpsIn = healingIn / duration, hpsOut = healingOut / duration)
}

abstract class Fight {
  val events:List[LogEvent]
  val title:Option[String]
  val startTime:Long
  val endTime:Long
  val duration:Int
  override def toString = title getOrElse ("@%d (%ds)" format (startTime, duration))
}
case class SingleFight(events: List[LogEvent], title: Option[String] = None) extends Fight {
  val startTime = if (events.isEmpty) 0 else events.head.time
  val endTime = if (events.isEmpty) 0 else events.last.time
  val duration: Int = (endTime - startTime).toInt
}

case class Fights(fights: List[Fight], title: Option[String] = None) extends Fight {
  lazy val events = fights.map(_.events).flatten
  lazy val startTime = fights.head.startTime
  lazy val endTime = fights.last.endTime
  lazy val duration = fights.map(_.duration).foldLeft(0)(_+_)
}

case class SpellBreakdown(damage: Int = 0, healing: Int = 0, hits: Int = 0, misses: Int = 0, crits: Int = 0) {
  def addDamage(amount: Int) = copy(damage = damage + amount)
  def addHealing(amount: Int) = copy(healing = healing + amount)
  def addHit() = copy(hits = hits + 1)
  def addMiss() = copy(misses = misses + 1)
  def addCrit() = copy(crits = crits + 1)
}
