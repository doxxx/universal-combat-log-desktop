package net.doxxx.universalcombatlog

import java.io.File
import scalax.io.{Codec, LongTraversable, Resource}
import util.matching.Regex
import java.util.{Date, Calendar}
import scala.Some

final class WoWParser extends LogParser {
  import Utils._

  private implicit val codec = Codec.UTF8
  private val parallelize = !java.lang.Boolean.getBoolean("nopar")

  def canLoad(f: File): Boolean = {
    Resource.fromFile(f).lines().headOption match {
      case Some(line) => parseLine(line).isDefined
      case _ => false
    }
  }

  def parse(f: File): List[LogEvent] = {
    parseLines(Resource.fromFile(f).lines())
  }

  def parseLines(lines: LongTraversable[String]): List[LogEvent] = {
    timeit("parseLines") {
      if (parallelize) {
        log("Parsing in parallel")
        lines.par.map(parseLine).toList.flatten
      }
      else {
        lines.map(parseLine).toList.flatten
      }
    }
  }

  // 8/31 21:49:35.954  SPELL_AURA_APPLIED,0x0700000004B0FBF1,"Snox-Terokkar",0x512,0x0,0x07000000049DBD4E,"Seleya-ShatteredHalls",0x514,0x0,116956,"Grace of Air",0x1,BUFF
  private val timestampRE = "([0-9]+)/([0-9]+) ([0-9]+):([0-9]+):([0-9]+)\\.([0-9]+)".r

  def parseLine(line: String): Option[LogEvent] = {
    timestampRE.findPrefixMatchOf(line) match {
      case Some(m) => {
        val time = parseTime(m)
        parseFields(time, line.substring(m.end).trim)
      }
      case None => None
    }
  }

  private def parseTime(m: Regex.Match): Long = {
    val month = m.group(1).toInt
    val day = m.group(2).toInt
    val hours = m.group(3).toInt
    val minutes = m.group(4).toInt
    val seconds = m.group(5).toInt
    val milliseconds = m.group(6).toInt

    // Initialize date with current time to use current year
    val date = Calendar.getInstance()
    date.setTime(new Date())

    // Overwrite other fields
    date.set(Calendar.MONTH, month)
    date.set(Calendar.DAY_OF_WEEK_IN_MONTH, day)
    date.set(Calendar.HOUR_OF_DAY, hours)
    date.set(Calendar.MINUTE, minutes)
    date.set(Calendar.SECOND, seconds)
    date.set(Calendar.MILLISECOND, milliseconds)

    // Return time in milliseconds since epoch (Unix time)
    date.getTimeInMillis
  }

  private def parseFields(time: Long, s: String): Option[LogEvent] = {
    val fields = splitFields(s)
    val it = fields.iterator

    // base fields
    val eventName = it.next()
    val actorID = parseHex(it.next().substring(2))
    val actorName = it.next()
    val actorFlags = parseHex(it.next().substring(2))
    val actorRaidFlags = parseHex(it.next().substring(2))
    val targetID = parseHex(it.next().substring(2))
    val targetName = it.next()
    val targetFlags = parseHex(it.next().substring(2))
    val targetRaidFlags = parseHex(it.next().substring(2))

    // extended fields depending on event
    var eventType: EventType.Value = EventType.Unrecognized
    var spell: String = ""
    var spellID: Long = 0
    var periodic: Boolean = false
    var amount: Int = 0
    var critical: Boolean = false

    eventName match {
      case "PARTY_KILL" => {
        eventType = EventType.Slain
      }
      case "UNIT_DIED" => {
        eventType = EventType.Died
      }
      case "UNIT_DESTROYED" => {
        // totems
        return None
      }
      case _ => {
        val eventNameParts = eventName.split("_", 2)

        if (eventNameParts.length != 2) {
          return None
        }

        eventNameParts(0) match {
          case "SWING" => {
            spell = "Auto Attack"
            spellID = -1
          }
          case "RANGE" => {
            spellID = it.next().toLong
            spell = it.next()
            it.next() // spell school
          }
          case "SPELL" => {
            spellID = it.next().toLong
            spell = it.next()
            it.next() // spell school
          }
          case "SPELL_PERIODIC" => {
            spellID = it.next().toLong
            spell = it.next()
            it.next() // spell school
            periodic = true
          }
        }

        eventNameParts(1) match {
          case "DAMAGE" => {
            eventType = if (periodic) EventType.DamageOverTime else EventType.DirectDamage
            amount = it.next().toInt
            it.next().toInt // overkill
            it.next() // school
            it.next() // resisted
            it.next() // blocked
            it.next() // absorbed
            critical = it.next() == "1"
            //it.next() // 1 == glancing
            //it.next() // 1 == crushing
          }
          case "MISSED" => {
            eventType = EventType.Miss
            //it.next() // missType
            //it.next() // isOffHand
            //it.next() // amountMissed
          }
          case "HEAL" => {
            amount = it.next().toInt
            it.next().toInt // overheal
            it.next().toInt // absorbed
            critical = it.next() == "1"
            eventType = if (critical) EventType.CritHeal else EventType.Heal
          }
          // ENERGIZE
          // DRAIN
          // LEECH
          // INTERRUPT
          // DISPEL
          // DISPEL_FAILED
          // STOLEN
          // EXTRA_ATTACKS
          case "AURA_APPLIED" => {
            val auraType = it.next()
            //it.next().toInt // amount
            auraType match {
              case "BUFF" => {
                eventType = EventType.BuffGain
              }
              case "DEBUFF" => {
                eventType = EventType.DebuffGain
              }
            }
          }
          case "AURA_REMOVED" => {
            val auraType = it.next()
            //it.next().toInt // amount
            auraType match {
              case "BUFF" => {
                eventType = EventType.BuffFade
              }
              case "DEBUFF" => {
                eventType = EventType.DebuffFade
              }
            }
          }
          // AURA_APPLIED_DOSE
          // AURA_REMOVED_DOSE
          // AURA_REFRESH
          // AURA_BROKEN
          // AURA_BROKEN_SPELL
          case "CAST_START" => {
            eventType = EventType.BeginCasting
          }
          // CAST_SUCCESS
          // CAST_FAILED
          // INSTAKILL
          // DURABILITY_DAMAGE
          // DURABILITY_DAMAGE_ALL
          // CREATE
          // SUMMON
          // RESURRECT
          case _ => {
            return None
          }
        }
      }
    }

    if (eventType == EventType.Unrecognized) {
      return None
    }

    // actor & target
    val actor: Option[Actor] = makeActor(actorID, actorName, actorFlags)
    val target: Option[Actor] = makeActor(targetID, targetName, targetFlags)

    if (!actor.isDefined || !target.isDefined) {
      return None
    }

    // Create log event
    Some(ActorEvent(time, eventType, actor.get, target.get, spell, spellID, amount, ""))
  }

  private def splitFields(s: String): Array[String] = {
    (s.split('"') map { s =>
      if (s.head == ',' || s.last == ',')
        s.split(',').filter(_.length > 0)
      else
        Array(s)
    }).flatten
  }

  private def parseHex(s: String): Long = {
    import java.lang.Long.parseLong
    if (s.length > 8) {
      val padded = s.padTo(16, '0')
      (parseLong(padded.substring(0, 8), 16) << 32) + parseLong(padded.substring(8), 16)
    }
    else {
      parseLong(s, 16)
    }
  }

  private def makeActor(id: Long, name: String, flags: Long): Option[Actor] = {
    val rel = entityRelationship(flags)
    objectType(flags) match {
      case TYPE_PLAYER => Some(getActor(PC(id, rel), NullActorID, Some(name)))
      // TODO: we don't have a way to associate pets with owners yet
      case TYPE_PET => Some(getActor(NPC(id, rel), NullActorID, Some(name)))
      case TYPE_NPC => Some(getActor(NPC(id, rel), NullActorID, Some(name)))
      case _ => None
    }
  }

  private def entityRelationship(flags: Long): Char = {
    affiliation(flags) match {
      case AFFILIATION_OUTSIDER => 'O'
      case AFFILIATION_RAID => 'R'
      case AFFILIATION_PARTY => 'P'
      case AFFILIATION_MINE => 'C'
      case _ => 'X'
    }
  }

  val TYPE_PLAYER = 0x400
  val TYPE_NPC = 0x800
  val TYPE_PET = 0x1000
  val TYPE_GUARDIAN = 0x2000
  val TYPE_OBJECT = 0x4000

  private def objectType(flags: Long): Long = flags & 0xFC00

  val CONTROLLER_PLAYER = 0x100
  val CONTROLLER_NPC = 0x200

  private def controller(flags: Long): Long = flags & 0x300

  val AFFILIATION_MINE = 0x1
  val AFFILIATION_PARTY = 0x2
  val AFFILIATION_RAID = 0x4
  val AFFILIATION_OUTSIDER = 0x8

  private def affiliation(flags: Long): Long = flags & 0xF
}
