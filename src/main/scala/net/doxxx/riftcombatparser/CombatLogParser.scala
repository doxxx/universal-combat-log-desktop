package net.doxxx.riftcombatparser

import io.Source
import util.matching.Regex
import java.lang.RuntimeException
import collection.mutable.{SynchronizedMap, HashMap}

object CombatLogParser {
  private val CombatToggleRE = new Regex("([0-9][0-9]:[0-9][0-9]:[0-9][0-9]) Combat (Begin|End)", "time", "toggle")
  private val DataRE =
    new Regex("([0-9]+) , (T=.+) , (T=.+) , (T=.+) , (T=.+) , (.*?) , (.*?) , (-?[0-9]*) , ([0-9]*) , (.*?)",
              "actorInfo", "targetInfo", "actorOwnerInfo", "targetOwnerInfo", "eventType", "actorName", "targetName",
              "amount", "spellId", "spell")
  private val LineRE = new Regex("([0-9][0-9]:[0-9][0-9]:[0-9][0-9]): \\( (.+?) \\) (.+)", "time", "data", "text")
  private val OverhealRE = new Regex("\\(([0-9]+) overheal\\)", "amount")

  sealed abstract class ActorID(val id: Long)
  case object NullActorID extends ActorID(0)
  case class NPC(override val id: Long) extends ActorID(id)
  case class PC(override val id: Long) extends ActorID(id)

  private val actors = new HashMap[ActorID, Actor] with SynchronizedMap[ActorID, Actor] {
    override def default(key: ActorID) = Nobody
  }

  def reset() {
    actors.clear()
  }

  def parse(source: Source): List[LogEvent] = {
    try {
      Utils.timeit("logparse") { () =>
        //source.getLines().toList.map(parseLine).toList.flatten
        source.getLines().toList.par.map(parseLine).toList.flatten
      }
    }
    finally {
      source.close()
    }
  }

  def playersAndPets: Set[Actor] = actors.filter {
    case (id: ActorID, player: Player) => true
    case (id: ActorID, playerPet: PlayerPet) => true
    case _ => false
  }.values.toSet

  def extractOverheal(text: String): Int = {
    OverhealRE.findFirstMatchIn(text) match {
      case Some(m) => m.group("amount").toInt
      case None => 0
    }
  }

  private def parseLine(line: String): Option[LogEvent] = {
    line match {
      case CombatToggleRE(time, toggle) => Some(CombatToggleEvent(parseTime(time), parseCombatToggle(toggle)))
      case LineRE(time, data, text) => parseActorEvent(time, data, text)
      case _ => {
        println("Unrecognized combat log line: " + line)
        None
      }
    }
  }

  private def parseTime(s: String): Long = {
    val parts = s.split(':')
    parts(0).toInt * 60 * 60 + parts(1).toInt * 60 + parts(2).toInt
  }

  private def parseCombatToggle(toggle: String): Boolean = toggle match {
    case "Begin" => true
    case "End" => false
    case _ => throw new IllegalArgumentException("Unrecognized combat toggle: " + toggle)
  }

  private def parseActorEvent(time: String, data: String, text: String): Option[ActorEvent] = {
    data match {
      case DataRE(eventType, actorInfo, targetInfo, actorOwnerInfo, targetOwnerInfo, actorName, targetName, amount, spellId, spell) =>
        Some(ActorEvent(parseTime(time), EventType(eventType.toInt),
          getActor(parseEntity(actorInfo), parseEntity(actorOwnerInfo), Some(actorName)),
          getActor(parseEntity(targetInfo), parseEntity(targetOwnerInfo), Some(targetName)),
          spell, spellId.toLong, amount.toInt, text))
      case _ => {
        println("Unrecognized data string: " + data)
        None
      }
    }
  }

  private def parseEntity(s: String): ActorID = {
    val parts = s.split('#')

    // T=P
    // T=N
    // T=X
    val t = parts(0).charAt(2)

    // R=C (character who collected combat log)
    // R=G (same group as C)
    // R=R (same raid as C)
    // R=O (others)
    //val r = parts(1).charAt(2)

    // 227009568756889439
    // 9223372041715776949 (when T=N, '9' is prepended to the ID)
    val id = if (t == 'N') {
      val (first, rest) = parts(2).splitAt(1)
      if (first != "9") {
        throw new RuntimeException("NPC ID does not start with 9: " + s)
      }
      else {
        rest.toLong
      }
    }
    else {
      parts(2).toLong
    }

    t match {
      case 'P' => PC(id)
      case 'N' => NPC(id)
      case 'X' => NullActorID
      case _ => throw new RuntimeException("Unrecognized entity type: " + s)
    }
  }

  private def getActor(actorID: ActorID, ownerID: ActorID, actorName: Option[String]): Actor = {
    actors.get(actorID) match {
      case Some(actor) => {
        actorName match {
          case Some(name) => {
            if (actor.name != name) {
              //println("Actor %s has changed name: %s -> %s".format(actorID, actor.name, name))
              actor.name = name
            }
          }
          case None => // nothing
        }
        ownerID match {
          case NullActorID => // nothing
          case _ => {
            val owner = actors(ownerID)
            actor match {
              case p: PlayerPet =>
                if (p.owner.id != owner.id)
                  throw new RuntimeException("%s has changed ownership: %s -> %s".format(p, p.owner, owner))
              case _ => throw new RuntimeException("%s cannot be owned by %s".format(actor, owner))
            }
          }
        }
        actor
      }
      case None => {
        actorID match {
          case pc: PC => {
            actors += actorID -> Player(pc, actorName.getOrElse("$dummy$"))
          }
          case npc: NPC => {
            val actor: Actor = ownerID match {
              case NullActorID => NonPlayer(npc, actorName.getOrElse("$dummy$"))
              case _ => PlayerPet(npc, actorName.getOrElse("$dummy$"), getPlayer(ownerID))
            }
            actors += actorID -> actor
          }
          case _ => // nothing
        }
        actors(actorID)
      }
    }
  }

 private def getPlayer(id: ActorID): Player = {
   getActor(id, NullActorID, None) match {
     case p: Player => p
     case a => throw new RuntimeException("Actor %s is not player".format(a))
   }
 }
}
