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
            val overheal = RiftParser.extractOverheal(ae.text)
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
      case ae: ActorEvent => {
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
    nonPlayerDamage.toMap
  }

  def isValidAction(ae: ActorEvent): Boolean = {
    (ae.actor.grouped ||
     ae.target.grouped ||
     ae.actor.isInstanceOf[NonPlayer] ||
     ae.target.isInstanceOf[NonPlayer]) &&
    (ae.eventType == EventTypes.Slain ||
     ae.eventType == EventTypes.Died ||
     ae.eventType == EventTypes.PowerGain ||
     !ae.spell.isEmpty)
  }

  val ignoredHostileSpells = Set("Sacrifice Life: Mana", "Critter Killer")

  def isHostileAction(ae: ActorEvent): Boolean = {
    (EventTypes.HostileTypes.contains(ae.eventType) &&
     !ignoredHostileSpells.contains(ae.spell) &&
     !(ae.actor.isInstanceOf[Player] && ae.target.isInstanceOf[Player]))
  }

  def deadEntity(ae: ActorEvent): Option[Entity] = {
    ae.eventType match {
      case Died => Some(ae.actor)
      case Slain => Some(ae.target)
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
    val pendingDeaths = new mutable.Queue[ActorEvent]

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
          else if (!currentFight.isEmpty && (ae.time - currentFight.last.time) >= 5000) {
            val f = SingleFight(currentFight.toList)
            debuglog("%d: 5 second timeout, creating fight: %s", ae.time, f.toString)
            finishFight(f)
          }

          deadEntity(ae) match {
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
                trackEntity(ae.actor)
                trackEntity(ae.target)
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
        (ae: ActorEvent) => {
          EventTypes.DamageTypes.contains(ae.eventType)
        }
      }
      else if (BreakdownType.HealingTypes.contains(breakdownType)) {
        (ae: ActorEvent) => {
          EventTypes.HealTypes.contains(ae.eventType)
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
          results(key) = results(key).addDamageType(RiftParser.extractDamageType(ae.text))
          totalDamage += ae.amount
        }
        else if (HealTypes.contains(ae.eventType)) {
          results(key) = results(key).addAmount(ae.amount)
          if (includeOverhealing) {
            val overheal = RiftParser.extractOverheal(ae.text)
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

  def filterByActors(events: List[LogEvent], actors: Set[Entity]) = {
    if (actors.isEmpty)
      events
    else
      events filter {
        case ae: ActorEvent => actors.contains(mergePetIntoOwner(ae.actor))
        case _ => true
      }
  }

  def filterByTarget(events: List[LogEvent], target: Entity): List[LogEvent] = {
    events filter {
      case ae: ActorEvent => target == ae.target
      case _ => true
    }
  }

  def entityDeaths(entity: Entity, events: List[LogEvent]): List[ActorEvent] = {
    val deaths = new mutable.ListBuffer[ActorEvent]
    for (event <- events) {
      event match {
        case ae: ActorEvent => {
          if (ae.eventType == Died) {
            if (ae.actor == entity) {
              deaths += ae
            }
          }
          else if (ae.eventType == Slain) {
            if (ae.target == entity) {
              deaths += ae
            }
          }
        }
        case _ => // skip
      }
    }
    deaths.toList
  }

  def eventsUpToDeath(death: ActorEvent, events: List[LogEvent], eventTypes: Set[EventTypes.Value] = Set.empty): List[LogEvent] = {
    val entity = deadEntity(death).get
    val eventsBeforeDeath = events.takeWhile(_.time <= death.time+1000)
    val withinTimeframe = eventsBeforeDeath.dropWhile(_.time < death.time - 10000)
    withinTimeframe.filter {
      case ae: ActorEvent => {
        ae == death ||
        (ae.actor == entity || ae.target == entity) &&
        (eventTypes.isEmpty || eventTypes.contains(ae.eventType)) &&
        ((EventTypes.HealTypes.contains(ae.eventType) && ae.actor != entity) ||
         (EventTypes.DamageTypes.contains(ae.eventType) && ae.target == entity))
      }
      case _ => false
    }
  }

  def chartHealthPriorToDeath(entity: Entity, events: List[LogEvent]): Array[Int] = {
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
          if (ae.target == entity) {
            if (EventTypes.DamageTypes.contains(ae.eventType)) {
              val overkill = RiftParser.extractOverkill(ae.text)
              health += ae.amount - overkill
            }
            else if (EventTypes.HealTypes.contains(ae.eventType)) {
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
        case ae: ActorEvent => {
          result.update(ae.spellId, ae.spell)
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
        case ae: ActorEvent => {
          result.update(ae.actor.id.id, ae.actor)
        }
        case _ => // nothing
      }
    }
    result.toMap
  }
}
