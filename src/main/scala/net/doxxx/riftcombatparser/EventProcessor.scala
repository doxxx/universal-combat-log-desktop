package net.doxxx.riftcombatparser

import EventType._
import collection.immutable.List._
import collection.mutable.HashMap
import java.util.prefs.Preferences

object EventProcessor {
  var includeOverhealing = false
  var useActorCombatTime = true
  var mergePetsIntoOwners = false

  def loadSettings(prefs: Preferences) {
    includeOverhealing = prefs.getBoolean("includeOverhealing", false)
    useActorCombatTime = prefs.getBoolean("useActorCombatTime", true)
    mergePetsIntoOwners = prefs.getBoolean("mergePetsIntoOwners", true)
  }

  def saveSettings(prefs: Preferences) {
    prefs.putBoolean("includeOverhealing", includeOverhealing)
    prefs.putBoolean("useActorCombatTime", useActorCombatTime)
    prefs.putBoolean("mergePetsIntoOwners", mergePetsIntoOwners)
  }

  def summary(fight: Fight, actors: Set[Actor]): Map[Actor, Summary] = {
    val results = new HashMap[Actor, Summary] {
      override def default(key: Actor) = Summary()
    }
    Utils.timeit("summary") { () =>
      for (e <- filterByActors(fight.events, actors)) e match {
        case ae: ActorEvent => {
          val actor = mergePetIntoOwner(ae.actor)
          val target = mergePetIntoOwner(ae.target)

          if (DamageTypes.contains(ae.eventType)) {
            results(actor) = results(actor).addDamageOut(ae.amount).updateTimes(ae.time)
            results(target) = results(target).addDamageIn(ae.amount)
          }
          else if (HealTypes.contains(ae.eventType)) {
            results(actor) = results(actor).addHealingOut(ae.amount).updateTimes(ae.time)
            results(target) = results(target).addHealingIn(ae.amount)
            val overheal = CombatLogParser.extractOverheal(ae.text)
            results(target) = results(target).addOverhealing(overheal)
            if (includeOverhealing) {
              results(actor) = results(actor).addHealingOut(overheal)
              results(target) = results(target).addHealingIn(overheal)
            }
          }
          else if (ae.eventType == Died) {
            results(actor) = results(actor).addDeath()
          }
          else if (ae.eventType == Slain) {
            results(target) = results(target).addDeath()
          }
        }
        case _ =>
      }
      for (actor <- results.keys) {
        results(actor) = results(actor).calculatePerSecond(fight.duration)
      }
    }
    results.toMap
  }

  def mergePetIntoOwner(actor: Actor): Actor = {
    actor match {
      case p: PlayerPet => if (mergePetsIntoOwners) p.owner else p
      case a: Actor => a
    }
  }

  def actorsSortedByActivity(events: List[LogEvent]): List[Actor] = {
    val activity = new HashMap[Actor, Int] {
      override def default(key: Actor) = 0
    }
    for (e <- events) e match {
      case ae: ActorEvent => {
        val actor = mergePetIntoOwner(ae.actor)
        activity(actor) = activity(actor) + 1
      }
      case _ =>
    }
    def cmp(a1:Pair[Actor,Int], a2:Pair[Actor,Int]) = {
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
    var totalDamage: Int = 0
    var totalHealing: Int = 0
    for (e <- events) e match {
      case ae: ActorEvent => {
        val spell = ae.actor match {
          case p: PlayerPet => "%s (%s)".format(ae.spell, p._name)
          case _ => ae.spell
        }
        if (DamageTypes.contains(ae.eventType)) {
          if (ae.eventType == CritDamage) {
            results(spell) = results(spell).addCrit()
            results(spell) = results(spell).addDamage(ae.amount)
          }
          else if (MissTypes.contains(ae.eventType)) {
            results(spell) = results(spell).addMiss()
          }
          else {
            results(spell) = results(spell).addDamage(ae.amount)
            results(spell) = results(spell).addHit()
          }
          totalDamage += ae.amount
        }
        else if (HealTypes.contains(ae.eventType)) {
          results(spell) = results(spell).addHealing(ae.amount)
          if (includeOverhealing) {
            val overheal = CombatLogParser.extractOverheal(ae.text)
            results(spell) = results(spell).addHealing(overheal)
          }
          if (ae.eventType == CritHeal) {
            results(spell) = results(spell).addCrit()
          }
          else {
            results(spell) = results(spell).addHit()
          }
          totalHealing += ae.amount
        }
      }
      case _ =>
    }
    for (spell <- results.keys) {
      if (results(spell).damage > 0) {
        results(spell) = results(spell).setPercentOfTotal(totalDamage)
      }
      else if (results(spell).healing > 0) {
        results(spell) = results(spell).setPercentOfTotal(totalHealing)
      }
    }
    results.toMap
  }

  def filterByActors(events: List[LogEvent], actors: Set[Actor]) = {
      if (actors.isEmpty)
        events
      else
        events filter {
          case ae: ActorEvent => actors.contains(mergePetIntoOwner(ae.actor))
          case _ => true
        }
  }

  def dpsSummary(data: Map[Actor, Summary]) = data.map {case (actor, summary) => actor -> summary.dpsOut}

  def dpsSorted(data: Map[Actor, Summary]) = dpsSummary(data).toList.sortBy {case (actor, value) => value}.reverse

  def hpsSummary(data: Map[Actor, Summary]) = data.map {case (actor, summary) => actor -> summary.hpsOut}

  def hpsSorted(data: Map[Actor, Summary]) = hpsSummary(data).toList.sortBy {case (actor, value) => value}.reverse

  def raidDPS(data: Map[Actor, Summary]) = dpsSummary(data).map {case (actor, value) => value}.sum

  def raidHPS(data: Map[Actor, Summary]) = hpsSummary(data).map {case (actor, value) => value}.sum

  def dpsSummaryForClipboard(data: Map[Actor, Summary]): String = {
    val dps = dpsSorted(data).take(10).filter {case (name, value) => value > 0}
    "DPS: Raid:%d - %s".format(
      raidDPS(data),
      (dps.map {case (name, value) => "%.4s:%d".format(name, value)}).mkString(", ")
    )
  }

  def hpsSummaryForClipboard(data: Map[Actor, Summary]): String = {
    val hps = hpsSorted(data).take(10).filter {case (name, value) => value > 0}
    "HPS: Raid:%d - %s".format(
      raidHPS(data),
      (hps.map {case (name, value) => "%.4s:%d".format(name, value)}).mkString(", ")
    )
  }

  def filterSummaryByActors(summary: Map[Actor, Summary], actors: Set[String]): Map[Actor, Summary] = {
    if (actors.isEmpty)
      summary
    else
      summary filter { case (actor, sum) => actors.contains(actor.name) }
  }
}

case class Summary(start: Long = Long.MaxValue, end: Long = 0, damageIn: Int = 0, dpsIn: Int = 0,
                   damageOut: Int = 0, dpsOut: Int = 0,
                   healingIn: Int = 0, hpsIn: Int = 0,
                   healingOut: Int = 0, hpsOut: Int = 0,
                   overhealing: Int = 0,
                   deaths: Int = 0) {
  def updateTimes(time: Long) = copy(scala.math.min(start, time), scala.math.max(end, time))
  def addDamageIn(amount: Int) = copy(damageIn = damageIn + amount)
  def addDamageOut(amount: Int) = copy(damageOut = damageOut + amount)
  def addHealingIn(amount: Int) = copy(healingIn = healingIn + amount)
  def addHealingOut(amount: Int) = copy(healingOut = healingOut + amount)
  def addOverhealing(amount: Int) = copy(overhealing = overhealing + amount)
  def addDeath() = copy(deaths = deaths + 1)
  def calculatePerSecond(fightDuration: Int) = {
    val d = if (EventProcessor.useActorCombatTime) duration else fightDuration
    if (d > 0)
      copy(dpsIn = damageIn / d, dpsOut = damageOut / d,
        hpsIn = healingIn / d, hpsOut = healingOut / d)
    else
      this
  }
  def duration = (end - start).toInt
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

case class SpellBreakdown(damage: Int = 0, healing: Int = 0, hits: Int = 0, misses: Int = 0, crits: Int = 0, percent: Int = 0) {
  def addDamage(amount: Int) = copy(damage = damage + amount)
  def addHealing(amount: Int) = copy(healing = healing + amount)
  def addHit() = copy(hits = hits + 1)
  def addMiss() = copy(misses = misses + 1)
  def addCrit() = copy(crits = crits + 1)
  def setPercentOfTotal(total: Int) =
    copy(percent = scala.math.round(scala.math.max(damage, healing).toDouble / total.toDouble * 100.0).toInt)
}
