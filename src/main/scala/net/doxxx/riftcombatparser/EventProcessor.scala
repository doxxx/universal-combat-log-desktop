package net.doxxx.riftcombatparser

import EventType._
import collection.immutable.List._
import java.util.prefs.Preferences
import collection.mutable
import annotation.tailrec

object EventProcessor {
  import Utils._

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

  def summary(fight: Fight): Map[Actor, Summary] = {
    val results = new mutable.HashMap[Actor, Summary] {
      override def default(key: Actor) = Summary()
    }
    timeit("summary") { () =>
      for (e <- fight.events) e match {
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
    val activity = new mutable.HashMap[Actor, Int] {
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

  def primaryNPC(events: List[LogEvent]): Option[String] = {
    val nonPlayerDamage = new mutable.HashMap[NonPlayer,Int]
    for (event <- events) {
      event match {
        case ae: ActorEvent if (DamageTypes.contains(ae.eventType)) => {
          ae.target match {
            case np: NonPlayer => {
              nonPlayerDamage(np) = nonPlayerDamage.getOrElse(np, 0) + ae.amount
            }
            case _ => // nothing
          }
        }
        case _ => // nothing
      }
    }
    if (nonPlayerDamage.isEmpty) return None
    implicit val ordering = new Ordering[(NonPlayer,Int)] {
      def compare(x: (NonPlayer, Int), y: (NonPlayer, Int)) = {
        x._2 - y._2
      }
    }
    Some(nonPlayerDamage.max(ordering)._1.name)
  }

  def isValidAction(ae: ActorEvent): Boolean = {
    (ae.actor.grouped ||
     ae.target.grouped ||
     ae.actor.isInstanceOf[NonPlayer] ||
     ae.target.isInstanceOf[NonPlayer]) &&
    (ae.eventType == EventType.Slain ||
     ae.eventType == EventType.Died ||
     ae.eventType == EventType.PowerGain ||
     !ae.spell.isEmpty)
  }

  val ignoredHostileSpells = Set("Sacrifice Life: Mana", "Critter Killer")

  def isHostileAction(ae: ActorEvent): Boolean = {
    (EventType.HostileTypes.contains(ae.eventType) &&
     !ignoredHostileSpells.contains(ae.spell) &&
     !(ae.actor.isInstanceOf[Player] && ae.target.isInstanceOf[Player]))
  }

  def deadActor(ae: ActorEvent): Option[Actor] = {
    ae.eventType match {
      case Died => Some(ae.actor)
      case Slain => Some(ae.target)
      case _ => None
    }
  }

  def extractNonPlayer(ae: ActorEvent): Option[NonPlayer] = {
    if (ae.actor.isInstanceOf[NonPlayer]) {
      Some(ae.actor.asInstanceOf[NonPlayer])
    }
    else if (ae.target.isInstanceOf[NonPlayer]) {
      Some(ae.target.asInstanceOf[NonPlayer])
    }
    else {
      None
    }
  }

  def extractPlayer(ae: ActorEvent): Option[Player] = {
    if (ae.actor.isInstanceOf[Player]) {
      Some(ae.actor.asInstanceOf[Player])
    }
    else if (ae.target.isInstanceOf[Player]) {
      Some(ae.target.asInstanceOf[Player])
    }
    else {
      None
    }
  }

  def splitFights(events: List[LogEvent]): List[Fight] = {
    val fights = new mutable.ArrayBuffer[Fight]
    val currentFight = new mutable.ArrayBuffer[LogEvent]
    val npcs = new mutable.HashSet[NonPlayer]
    val deadNPCs = new mutable.HashSet[NonPlayer]
    val pcs = new mutable.HashSet[Player]
    val deadPCs = new mutable.HashSet[Player]
    val pendingDeaths = new mutable.Queue[ActorEvent]

    def processPendingDeaths(time: Long) {
      while (!pendingDeaths.isEmpty && pendingDeaths.front.time < time) {
        val death = pendingDeaths.dequeue()
        currentFight += death
        val actor = deadActor(death)
        actor match {
          case Some(np: NonPlayer) => {
            debuglog("%d: Processing NPC death: %s", death.time, np.toString)
            npcs += np
            deadNPCs += np
          }
          case Some(p: Player) => {
            debuglog("%d: Processing player death: %s", death.time, p.name)
            pcs += p
            deadPCs += p
          }
          case Some(pp: PlayerPet) => {
            debuglog("%d: Processing player pet death: %s", death.time, pp.toString)
          }
          case Some(Nobody) => {
            debuglog("Nobody died?!")
          }
          case None => {
            debuglog("Death event which contains no dead actor?!")
          }
        }
      }
    }

    def trackActor(a: Actor) {
      a match {
        case np: NonPlayer => npcs += np
        case p: Player => pcs += p
        case _ => // do nothing
      }
    }

    def finishFight(f: SingleFight) {
      fights += f
      currentFight.clear()
      npcs.clear()
      deadNPCs.clear()
      pcs.clear()
      deadPCs.clear()
    }

    for (event <- events) {
      event match {
        case ae: ActorEvent if (isValidAction(ae)) => {
          // process queued deaths
          processPendingDeaths(ae.time)

          // if all active NPCs have died, fight is finished
          if (!npcs.isEmpty && npcs == deadNPCs && !currentFight.isEmpty) {
            val f = SingleFight(currentFight.toList)
            debuglog("%d: All active NPCs have died, creating fight: %s", ae.time, f.toString)
            finishFight(f)
          }
          // if all active PCs have died, fight is finished
          else if (!pcs.isEmpty && pcs == deadPCs && !currentFight.isEmpty) {
            val f = SingleFight(currentFight.toList)
            debuglog("%d: All active PCs have died, creating fight: %s", ae.time, f.toString)
            finishFight(f)
          }
          // if more than 5 seconds have passed since the last hostile fight event, fight is finished
          else if (!currentFight.isEmpty && (ae.time - currentFight.last.time) >= 5) {
            val f = SingleFight(currentFight.toList)
            debuglog("%d: 5 second timeout, creating fight: %s", ae.time, f.toString)
            finishFight(f)
          }

          val actor = deadActor(ae)
          actor match {
            case Some(a) => {
              // if it was a death event, queue it, since they occur before the actual killing blow
              //debuglog("%d: Queuing actor death: %s", ae.time, a.toString)
              pendingDeaths.enqueue(ae)
            }
            case None => {
              // if it was a hostile action or a fight is in progress, add it to the current fight
              if (isHostileAction(ae) || !currentFight.isEmpty) {
                if (currentFight.isEmpty) {
                  debuglog("%d: First hostile action: %s", ae.time, ae.toString)
                }
                // non-death event
                currentFight += ae
                trackActor(ae.actor)
                trackActor(ae.target)
              }
            }
          }
        }
        case _ => // do nothing
      }
    }

    processPendingDeaths(Long.MaxValue)

    if (!currentFight.isEmpty) {
      val f = SingleFight(currentFight.toList)
      debuglog("Events remain, creating fight: %s", f.toString)
      finishFight(f)
    }

    fights.toList
  }

  private val dayTime = 24*60*60
  def normalizeTimes(events: List[LogEvent]): List[LogEvent] = {
    if (events.isEmpty) return events
    val startTime = events.head.time
    @tailrec
    def normalize(result: List[LogEvent], offset: Long, prev: LogEvent, events: List[LogEvent]): List[LogEvent] = {
      events match {
        case Nil => result
        case current :: rest => {
          if (current.time < prev.time) {
            normalize(current.copy(current.time + offset + dayTime) :: result, offset + dayTime, current, rest)
          }
          else {
            normalize(current.copy(current.time + offset) :: result, offset, current, rest)
          }
        }
      }
    }
    normalize(Nil, -startTime, events.head, events).reverse
  }

  def breakdown(breakdownType: BreakdownType.Value, player: Actor, events: List[LogEvent]): Map[String, Breakdown] = {
    val filteredEvents =
      if (BreakdownType.OutgoingTypes.contains(breakdownType)) {
        filterByActors(events, Set(player))
      }
      else if (BreakdownType.IncomingTypes.contains(breakdownType)) {
        filterByTarget(events, player)
      }
      else throw new RuntimeException("Invalid breakdown type")

    val healingDamageFilter =
      if (BreakdownType.DamageTypes.contains(breakdownType)) {
        (ae: ActorEvent) => {
          EventType.DamageTypes.contains(ae.eventType)
        }
      }
      else if (BreakdownType.HealingTypes.contains(breakdownType)) {
        (ae: ActorEvent) => {
          EventType.HealTypes.contains(ae.eventType)
        }
      }
      else {
        throw new RuntimeException("Invalid breakdown type")
      }

    val extractKey =
      if (BreakdownType.BySpellTypes.contains(breakdownType)) {
        (ae: ActorEvent) => {
          ae.actor match {
            case p: PlayerPet => "%s (%s)".format(ae.spell, p._name)
            case _ => ae.spell
          }
        }
      }
      else if (BreakdownType.ByTargetTypes.contains(breakdownType)) {
        (ae: ActorEvent) => {
          ae.target.name
        }
      }
      else if (BreakdownType.ByActorTypes.contains(breakdownType)) {
        (ae: ActorEvent) => {
          ae.actor.name
        }
      }
      else throw new RuntimeException("Invalid breakdown type")

    val results = new mutable.HashMap[String, Breakdown] {
      override def default(key: String) = Breakdown()
    }
    var totalDamage: Int = 0
    var totalHealing: Int = 0

    for (e <- filteredEvents) e match {
      case ae: ActorEvent if (healingDamageFilter(ae)) => {
        val key = extractKey(ae)
        if (DamageTypes.contains(ae.eventType)) {
          if (ae.eventType == CritDamage) {
            results(key) = results(key).addCrit()
            results(key) = results(key).addAmount(ae.amount)
          }
          else if (MissTypes.contains(ae.eventType)) {
            results(key) = results(key).addMiss()
          }
          else {
            results(key) = results(key).addAmount(ae.amount)
            results(key) = results(key).addHit()
          }
          results(key) = results(key).addDamageType(CombatLogParser.extractDamageType(ae.text))
          totalDamage += ae.amount
        }
        else if (HealTypes.contains(ae.eventType)) {
          results(key) = results(key).addAmount(ae.amount)
          if (includeOverhealing) {
            val overheal = CombatLogParser.extractOverheal(ae.text)
            results(key) = results(key).addAmount(overheal)
          }
          if (ae.eventType == CritHeal) {
            results(key) = results(key).addCrit()
          }
          else {
            results(key) = results(key).addHit()
          }
          totalHealing += ae.amount
        }
      }
      case _ =>
    }

    if (BreakdownType.DamageTypes.contains(breakdownType)) {
      for (spell <- results.keys) {
        results(spell) = results(spell).setPercentOfTotal(totalDamage)
      }
    }
    else if (BreakdownType.HealingTypes.contains(breakdownType)) {
      for (spell <- results.keys) {
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

  def filterByTarget(events: List[LogEvent], target: Actor): List[LogEvent] = {
    events filter {
      case ae: ActorEvent => target == ae.target
      case _ => true
    }
  }

  def actorDeaths(actor: Actor, events: List[LogEvent]): List[ActorEvent] = {
    val deaths = new mutable.ListBuffer[ActorEvent]
    for (event <- events) {
      event match {
        case ae: ActorEvent => {
          if (ae.eventType == Died) {
            if (ae.actor == actor) {
              deaths += ae
            }
          }
          else if (ae.eventType == Slain) {
            if (ae.target == actor) {
              deaths += ae
            }
          }
        }
        case _ => // skip
      }
    }
    deaths.toList
  }

  def eventsUpToDeath(death: ActorEvent, events: List[LogEvent], eventTypes: Set[EventType.Value] = Set.empty): List[LogEvent] = {
    val actor = death.eventType match {
      case Died => death.actor
      case Slain => death.target
    }
    val eventsBeforeDeath = events.takeWhile(_.time <= death.time+1)
    val withinTimeframe = eventsBeforeDeath.dropWhile(_.time < death.time - 10)
    withinTimeframe.filter {
      case ae: ActorEvent => {
        ae == death ||
        (ae.actor == actor || ae.target == actor) &&
        (eventTypes == Nil || eventTypes.contains(ae.eventType)) &&
        ((EventType.HealTypes.contains(ae.eventType) && ae.actor != actor) ||
         (EventType.DamageTypes.contains(ae.eventType) && ae.target == actor))
      }
      case _ => false
    }
  }

  def chartHealthPriorToDeath(actor: Actor, events: List[LogEvent]): Array[Int] = {
    val chart = new mutable.ArrayBuffer[Int]
    val rev = events.reverse
    var health: Int = 0
    var time = rev.head.time
    chart += 0
    for (e <- rev) {
      if (e.time < time) {
        chart += health
        time = e.time
      }
      e match {
        case ae: ActorEvent => {
          if (ae.target == actor) {
            if (EventType.DamageTypes.contains(ae.eventType)) {
              val overkill = CombatLogParser.extractOverkill(ae.text)
              health += ae.amount - overkill
            }
            else if (EventType.HealTypes.contains(ae.eventType)) {
              health -= ae.amount
            }
          }
        }
        case _ => // do nothing
      }
    }
    chart += health
    chart.reverse.toArray
  }

  def dpsSummary(data: Map[Actor, Summary]) = data.map {case (actor, summary) => actor -> summary.dpsOut}

  def dpsSorted(data: Map[Actor, Summary]) = dpsSummary(data).toList.sortBy {case (actor, value) => value}.reverse

  def hpsSummary(data: Map[Actor, Summary]) = data.map {case (actor, summary) => actor -> summary.hpsOut}

  def hpsSorted(data: Map[Actor, Summary]) = hpsSummary(data).toList.sortBy {case (actor, value) => value}.reverse

  def raidDPS(data: Map[Actor, Summary]) = dpsSummary(data).map {case (actor, value) => value}.sum

  def raidHPS(data: Map[Actor, Summary]) = hpsSummary(data).map {case (actor, value) => value}.sum

  def dpsSummaryForClipboard(data: Map[Actor, Summary]): String = {
    val dps = dpsSorted(data).take(10).filter {case (actor, value) => value > 0}
    "DPS: Raid:%d - %s".format(
      raidDPS(data),
      (dps.map {case (actor, value) => "%.4s:%d".format(actor.name, value)}).mkString(", ")
    )
  }

  def hpsSummaryForClipboard(data: Map[Actor, Summary]): String = {
    val hps = hpsSorted(data).take(10).filter {case (actor, value) => value > 0}
    "HPS: Raid:%d - %s".format(
      raidHPS(data),
      (hps.map {case (actor, value) => "%.4s:%d".format(actor.name, value)}).mkString(", ")
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
                   deaths: Int = 0,
                   duration: Int = 0) {
  def updateTimes(time: Long) = copy(start = scala.math.min(start, time), end = scala.math.max(end, time))
  def addDamageIn(amount: Int) = copy(damageIn = damageIn + amount)
  def addDamageOut(amount: Int) = copy(damageOut = damageOut + amount)
  def addHealingIn(amount: Int) = copy(healingIn = healingIn + amount)
  def addHealingOut(amount: Int) = copy(healingOut = healingOut + amount)
  def addOverhealing(amount: Int) = copy(overhealing = overhealing + amount)
  def addDeath() = copy(deaths = deaths + 1, duration = duration + (end - start).toInt, start = Long.MaxValue, end = 0)
  def combatTime: Int = {
    duration + {
      if (start == Long.MaxValue && end == 0)
        0
      else
        (end - start).toInt
    }
  }
  def calculatePerSecond(fightDuration: Int) = {
    val d = if (EventProcessor.useActorCombatTime) combatTime else fightDuration
    if (d > 0)
      copy(dpsIn = damageIn / d, dpsOut = damageOut / d,
        hpsIn = healingIn / d, hpsOut = healingOut / d)
    else
      this
  }
}

abstract class Fight {
  def events:List[LogEvent]
  def title:Option[String]
  def startTime:Long
  def endTime:Long
  def duration:Int
  override def toString = "%s (%ds)".format(title.getOrElse("@%d".format(startTime)), duration)
}
case class EmptyFight(time: Long = 0) extends Fight {
  val events = Nil
  val title = None
  val startTime = time
  val endTime = time
  val duration = 0
}
case class SingleFight(events: List[LogEvent]) extends Fight {
  val title = EventProcessor.primaryNPC(events)
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

case class Breakdown(amount: Int = 0, hits: Int = 0, misses: Int = 0, crits: Int = 0, damageTypes: Set[String] = Set.empty, percent: Int = 0) {
  def addAmount(newAmount: Int) = copy(amount = amount + newAmount)
  def addHit() = copy(hits = hits + 1)
  def addMiss() = copy(misses = misses + 1)
  def addCrit() = copy(crits = crits + 1)
  def addDamageType(damageType: String) = copy(damageTypes = damageTypes + damageType)
  def setPercentOfTotal(total: Int) =
    copy(percent = scala.math.round(amount.toDouble / total.toDouble * 100.0).toInt)
}
