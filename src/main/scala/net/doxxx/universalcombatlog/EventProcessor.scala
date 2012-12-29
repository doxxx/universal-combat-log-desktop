package net.doxxx.universalcombatlog

import EventTypes._
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

  def summary(fight: Fight): Map[Entity, Summary] = {
    val results = new mutable.HashMap[Entity, Summary] {
      override def default(key: Entity) = Summary()
    }
    timeit("summary") {
      for (event <- fight.events) event match {
        case ce: CombatEvent => {
          val actor = mergePetIntoOwner(ce.actor)
          val target = mergePetIntoOwner(ce.target)

          if (DamageTypes.contains(ce.eventType)) {
            results(actor) = results(actor).addDamageOut(ce.amount).updateTimes(ce.time)
            results(target) = results(target).addDamageIn(ce.amount)
          }
          else if (HealTypes.contains(ce.eventType)) {
            results(actor) = results(actor).addHealingOut(ce.amount).updateTimes(ce.time)
            results(target) = results(target).addHealingIn(ce.amount)
            val overheal = ce.overAmount
            results(target) = results(target).addOverhealing(overheal)
            if (includeOverhealing) {
              results(actor) = results(actor).addHealingOut(overheal)
              results(target) = results(target).addHealingIn(overheal)
            }
          }
          else if (ce.eventType == Died) {
            results(actor) = results(actor).addDeath()
          }
          else if (ce.eventType == Slain) {
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

  def mergePetIntoOwner(entity: Entity): Entity = {
    entity match {
      case p: PlayerPet => if (mergePetsIntoOwners) p.owner else p
      case a: Entity => a
    }
  }

  def actorsSortedByActivity(events: List[LogEvent]): List[Entity] = {
    val activity = new mutable.HashMap[Entity, Int] {
      override def default(key: Entity) = 0
    }
    for (e <- events) e match {
      case ae: CombatEvent => {
        val actor = mergePetIntoOwner(ae.actor)
        activity(actor) = activity(actor) + 1
      }
      case _ =>
    }
    def cmp(a1:Pair[Entity,Int], a2:Pair[Entity,Int]) = {
      a1._2 > a2._2
    }
    for ((k,v) <- activity.toList.sortWith(cmp)) yield k
  }

  def primaryNPC(events: List[LogEvent]): Option[String] = {
    val nonPlayerDamage = sumNonPlayerDamage(events)
    if (nonPlayerDamage.isEmpty) return None
    implicit val ordering = new Ordering[(NonPlayer,Int)] {
      def compare(x: (NonPlayer, Int), y: (NonPlayer, Int)) = {
        x._2 - y._2
      }
    }
    Some(nonPlayerDamage.max(ordering)._1.name)
  }


  def sumNonPlayerDamage(events: scala.List[LogEvent]): Map[NonPlayer, Int] = {
    val nonPlayerDamage = new mutable.HashMap[NonPlayer, Int]
    for (event <- events) {
      event match {
        case ce: CombatEvent if (DamageTypes.contains(ce.eventType)) => {
          ce.target match {
            case np: NonPlayer => {
              nonPlayerDamage(np) = nonPlayerDamage.getOrElse(np, 0) + ce.amount
            }
            case _ => // nothing
          }
        }
        case _ => // nothing
      }
    }
    nonPlayerDamage.toMap
  }

  def isValidAction(event: CombatEvent): Boolean = {
    (event.actor.grouped ||
     event.target.grouped ||
     event.actor.isInstanceOf[NonPlayer] ||
     event.target.isInstanceOf[NonPlayer]) &&
    (event.eventType == EventTypes.Slain ||
     event.eventType == EventTypes.Died ||
     event.eventType == EventTypes.PowerGain ||
     !event.spell.isEmpty)
  }

  val ignoredHostileSpells = Set("Sacrifice Life: Mana", "Critter Killer")

  def isHostileAction(event: CombatEvent): Boolean = {
    (EventTypes.HostileTypes.contains(event.eventType) &&
     !ignoredHostileSpells.contains(event.spell) &&
     !(event.actor.isInstanceOf[Player] && event.target.isInstanceOf[Player]))
  }

  def deadEntity(event: CombatEvent): Option[Entity] = {
    event.eventType match {
      case Died => Some(event.actor)
      case Slain => Some(event.target)
      case _ => None
    }
  }

  def splitFights(events: List[LogEvent]): List[Fight] = {
    val fights = new mutable.ArrayBuffer[Fight]
    val currentFight = new mutable.ArrayBuffer[LogEvent]
    val npcs = new mutable.HashSet[NonPlayer]
    val deadNPCs = new mutable.HashSet[NonPlayer]
    val pcs = new mutable.HashSet[Player]
    val deadPCs = new mutable.HashSet[Player]
    val pendingDeaths = new mutable.Queue[CombatEvent]

    def processPendingDeaths(time: Long) {
      while (!pendingDeaths.isEmpty && pendingDeaths.front.time < time) {
        val death = pendingDeaths.dequeue()
        currentFight += death
        val actor = deadEntity(death)
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

    def trackEntity(a: Entity) {
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
        case ce: CombatEvent if (isValidAction(ce)) => {
          // process queued deaths
          processPendingDeaths(ce.time)

          // if all active NPCs have died, fight is finished
          if (!npcs.isEmpty && npcs == deadNPCs && !currentFight.isEmpty) {
            val f = SingleFight(currentFight.toList)
            debuglog("%d: All active NPCs have died, creating fight: %s", ce.time, f.toString)
            finishFight(f)
          }
          // if all active PCs have died, fight is finished
          else if (!pcs.isEmpty && pcs == deadPCs && !currentFight.isEmpty) {
            val f = SingleFight(currentFight.toList)
            debuglog("%d: All active PCs have died, creating fight: %s", ce.time, f.toString)
            finishFight(f)
          }
          // if more than 5 seconds have passed since the last hostile fight event, fight is finished
          else if (!currentFight.isEmpty && (ce.time - currentFight.last.time) >= 5000) {
            val f = SingleFight(currentFight.toList)
            debuglog("%d: 5 second timeout, creating fight: %s", ce.time, f.toString)
            finishFight(f)
          }

          deadEntity(ce) match {
            case Some(a) => {
              // if it was a death event, queue it, since they occur before the actual killing blow
              //debuglog("%d: Queuing actor death: %s", ae.time, a.toString)
              pendingDeaths.enqueue(ce)
            }
            case None => {
              // if it was a hostile action or a fight is in progress, add it to the current fight
              if (isHostileAction(ce) || !currentFight.isEmpty) {
                if (currentFight.isEmpty) {
                  debuglog("%d: First hostile action: %s", ce.time, ce.toString)
                }
                // non-death event
                currentFight += ce
                trackEntity(ce.actor)
                trackEntity(ce.target)
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

  private val dayTime = 24*60*60*1000
  def normalizeTimes(events: List[LogEvent], startTime: Long): List[LogEvent] = {
    if (events.isEmpty) return events
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
    normalize(Nil, startTime - events.head.time, events.head, events).reverse
  }

  def breakdown(breakdownType: BreakdownType.Value, player: Entity, events: List[LogEvent]): Map[String, Breakdown] = {
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
        (event: CombatEvent) => {
          EventTypes.DamageTypes.contains(event.eventType)
        }
      }
      else if (BreakdownType.HealingTypes.contains(breakdownType)) {
        (event: CombatEvent) => {
          EventTypes.HealTypes.contains(event.eventType)
        }
      }
      else {
        throw new RuntimeException("Invalid breakdown type")
      }

    val extractKey =
      if (BreakdownType.BySpellTypes.contains(breakdownType)) {
        (event: CombatEvent) => {
          event.actor match {
            case p: PlayerPet => "%s (%s)".format(event.spell, p._name)
            case _ => event.spell
          }
        }
      }
      else if (BreakdownType.ByTargetTypes.contains(breakdownType)) {
        (event: CombatEvent) => {
          event.target.name
        }
      }
      else if (BreakdownType.ByActorTypes.contains(breakdownType)) {
        (event: CombatEvent) => {
          event.actor.name
        }
      }
      else throw new RuntimeException("Invalid breakdown type")

    val results = new mutable.HashMap[String, Breakdown] {
      override def default(key: String) = Breakdown()
    }
    var totalDamage: Int = 0
    var totalHealing: Int = 0

    for (event <- filteredEvents) event match {
      case ce: CombatEvent if (healingDamageFilter(ce)) => {
        val key = extractKey(ce)
        if (DamageTypes.contains(ce.eventType)) {
          if (ce.eventType == CritDamage) {
            results(key) = results(key).addCrit()
            results(key) = results(key).addAmount(ce.amount)
          }
          else if (MissTypes.contains(ce.eventType)) {
            results(key) = results(key).addMiss()
          }
          else {
            results(key) = results(key).addAmount(ce.amount)
            results(key) = results(key).addHit()
          }
          results(key) = results(key).addDamageType(RiftParser.extractDamageType(ce.text))
          totalDamage += ce.amount
        }
        else if (HealTypes.contains(ce.eventType)) {
          results(key) = results(key).addAmount(ce.amount)
          if (includeOverhealing) {
            val overheal = ce.overAmount
            results(key) = results(key).addAmount(overheal)
          }
          if (ce.eventType == CritHeal) {
            results(key) = results(key).addCrit()
          }
          else {
            results(key) = results(key).addHit()
          }
          totalHealing += ce.amount
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

  def filterByActors(events: List[LogEvent], actors: Set[Entity]) = {
    if (actors.isEmpty)
      events
    else
      events filter {
        case event: CombatEvent => actors.contains(mergePetIntoOwner(event.actor))
        case _ => true
      }
  }

  def filterByTarget(events: List[LogEvent], target: Entity): List[LogEvent] = {
    events filter {
      case event: CombatEvent => target == event.target
      case _ => true
    }
  }

  def entityDeaths(entity: Entity, events: List[LogEvent]): List[CombatEvent] = {
    val deaths = new mutable.ListBuffer[CombatEvent]
    for (event <- events) {
      event match {
        case ce: CombatEvent => {
          if (ce.eventType == Died) {
            if (ce.actor == entity) {
              deaths += ce
            }
          }
          else if (ce.eventType == Slain) {
            if (ce.target == entity) {
              deaths += ce
            }
          }
        }
        case _ => // skip
      }
    }
    deaths.toList
  }

  def eventsUpToDeath(death: CombatEvent, events: List[LogEvent], eventTypes: Set[EventTypes.Value] = Set.empty): List[LogEvent] = {
    val entity = deadEntity(death).get
    val eventsBeforeDeath = events.takeWhile(_.time <= death.time+1000)
    val withinTimeframe = eventsBeforeDeath.dropWhile(_.time < death.time - 10000)
    withinTimeframe.filter {
      case event: CombatEvent => {
        event == death ||
        (event.actor == entity || event.target == entity) &&
        (eventTypes.isEmpty || eventTypes.contains(event.eventType)) &&
        ((EventTypes.HealTypes.contains(event.eventType) && event.actor != entity) ||
         (EventTypes.DamageTypes.contains(event.eventType) && event.target == entity))
      }
      case _ => false
    }
  }

  def chartHealthPriorToDeath(entity: Entity, events: List[LogEvent]): Array[Int] = {
    val chart = new mutable.ArrayBuffer[Int]
    val revEvents = events.reverse
    var health: Int = 0
    var time = revEvents.head.time
    chart += 0
    for (event <- revEvents) {
      if (event.time < time) {
        chart += health
        time = event.time
      }
      event match {
        case ce: CombatEvent => {
          if (ce.target == entity) {
            if (EventTypes.DamageTypes.contains(ce.eventType)) {
              health += ce.amount
            }
            else if (EventTypes.HealTypes.contains(ce.eventType)) {
              health -= ce.amount
            }
          }
        }
        case _ => // do nothing
      }
    }
    chart += health
    chart.reverse.toArray
  }

  def dpsSummary(data: Map[Entity, Summary]) = data.map {case (entity, summary) => entity -> summary.dpsOut}

  def dpsSorted(data: Map[Entity, Summary]) = dpsSummary(data).toList.sortBy {case (entity, value) => value}.reverse

  def hpsSummary(data: Map[Entity, Summary]) = data.map {case (entity, summary) => entity -> summary.hpsOut}

  def hpsSorted(data: Map[Entity, Summary]) = hpsSummary(data).toList.sortBy {case (entity, value) => value}.reverse

  def raidDPS(data: Map[Entity, Summary]) = dpsSummary(data).map {case (entity, value) => value}.sum

  def raidHPS(data: Map[Entity, Summary]) = hpsSummary(data).map {case (entity, value) => value}.sum

  def dpsSummaryForClipboard(data: Map[Entity, Summary]): String = {
    val dps = dpsSorted(data).take(10).filter {case (entity, value) => value > 0}
    "DPS: Raid:%d - %s".format(
      raidDPS(data),
      (dps.map {case (entity, value) => "%.4s:%d".format(entity.name, value)}).mkString(", ")
    )
  }

  def hpsSummaryForClipboard(data: Map[Entity, Summary]): String = {
    val hps = hpsSorted(data).take(10).filter {case (entity, value) => value > 0}
    "HPS: Raid:%d - %s".format(
      raidHPS(data),
      (hps.map {case (entity, value) => "%.4s:%d".format(entity.name, value)}).mkString(", ")
    )
  }

  def filterSummaryByEntities(summary: Map[Entity, Summary], entities: Set[String]): Map[Entity, Summary] = {
    if (entities.isEmpty)
      summary
    else
      summary filter { case (entity, sum) => entities.contains(entity.name) }
  }

  def spellIndex(events: List[LogEvent]): Map[Long,String] = {
    val result = new mutable.HashMap[Long,String]()
    for (event <- events) {
      event match {
        case ce: CombatEvent => {
          result.update(ce.spellId, ce.spell)
        }
        case _ => // nothing
      }
    }
    result.toMap
  }

  def entityIndex(events: List[LogEvent]): Map[Long,Entity] = {
    val result = new mutable.HashMap[Long,Entity]()
    for (event <- events) {
      event match {
        case ce: CombatEvent => {
          result.update(ce.actor.id.id, ce.actor)
        }
        case _ => // nothing
      }
    }
    result.toMap
  }
}
