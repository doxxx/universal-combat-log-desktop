package net.doxxx.riftcombatparser

import io.Source
import util.matching.Regex
import java.io._
import java.lang.{Boolean, RuntimeException}
import java.util.concurrent.locks.ReentrantReadWriteLock
import collection.mutable

object CombatLogParser {
  import Utils._

  private val parallelize = !Boolean.getBoolean("nopar")

  private val CombatToggleRE = new Regex("([0-9][0-9]:[0-9][0-9]:[0-9][0-9]) Combat (Begin|End)", "time", "toggle")
  private val DataRE =
    new Regex("([0-9]+) , (T=.+) , (T=.+) , (T=.+) , (T=.+) , (.*?) , (.*?) , (-?[0-9]*) , ([0-9]*) , (.*?)",
              "actorInfo", "targetInfo", "actorOwnerInfo", "targetOwnerInfo", "eventType", "actorName", "targetName",
              "amount", "spellId", "spell")
  private val LineRE = new Regex("([0-9][0-9]:[0-9][0-9]:[0-9][0-9]): \\( (.+?) \\) (.+)", "time", "data", "text")
  private val OverhealRE = new Regex("([0-9]+) overheal", "amount")
  private val OverkillRE = new Regex("([0-9]+) overkill", "amount")
  private val AbsorbedRE = new Regex("([0-9]+) absorbed", "amount")
  private val DamageTypeRE = new Regex("[0-9]+ (.+) damage", "type")

  sealed abstract class ActorID {
    def id: Long
    def rel: Char
  }
  case object NullActorID extends ActorID {
    val id = 0L
    val rel = 'X'
  }
  case class NPC(id: Long, rel: Char) extends ActorID
  case class PC(id: Long, rel: Char) extends ActorID

  private val actorsLock = new ReentrantReadWriteLock()
  private val actors = new mutable.HashMap[ActorID, Actor] {
    override def default(key: ActorID) = Nobody
  }
  private var lastFilePos: Long = 0
  private var lastEvents: List[LogEvent] = Nil
  private var threads: Set[String] = Set.empty

  def reset() {
    actors.clear()
    lastFilePos = 0
    lastEvents = Nil
  }

  def parse(source: Source): List[LogEvent] = {
    try {
      timeit("logparse") {
        parse(source.getLines().toList)
      }
    }
    finally {
      source.close()
    }
  }

  def parse(file: File): List[LogEvent] = {
    val raf = new RandomAccessFile(file, "r")
    if (raf.length < lastFilePos) {
      log("File size smaller than last position; resetting")
      reset()
    }
    else {
      log("Seeking to last position: %d", lastFilePos)
      raf.seek(lastFilePos)
    }

    val data:Array[Byte] = Array.ofDim((raf.length - raf.getFilePointer).toInt)
    timeit("readFully") {
      log("Reading %d bytes", data.length)
      raf.readFully(data)
    }

    lastFilePos = raf.getFilePointer

    log("Saved last position: %d", lastFilePos)

    raf.close()

    val reader = new BufferedReader(new InputStreamReader(new ByteArrayInputStream(data)))
    var lines = new mutable.ListBuffer[String]
    var done = false

    timeit("readLines") {
      while (!done) {
        val line = reader.readLine()
        if (line == null) {
          done = true
        }
        else {
          lines += line
        }
      }
    }

    log("Read %d lines", lines.length)

    threads = Set.empty

    val newEvents = timeit("parseLines") {
      parse(lines.toList)
    }

    log("Parsed %d events using %d threads", newEvents.length, threads.size)

    lastEvents = lastEvents ::: newEvents

    lastEvents
  }

  def parse(lines: List[String]): List[LogEvent] = {
    if (parallelize) {
      log("Parsing in parallel")
      lines.par.map(parseLine).toList.flatten
    }
    else {
      lines.map(parseLine).toList.flatten
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

  def extractOverkill(text: String): Int = {
    OverkillRE.findFirstMatchIn(text) match {
      case Some(m) => m.group("amount").toInt
      case None => 0
    }
  }

  def extractAbsorbed(text: String): Int = {
    AbsorbedRE.findFirstMatchIn(text) match {
      case Some(m) => m.group("amount").toInt
      case None => 0
    }
  }

  def extractDamageType(text: String): String = {
    DamageTypeRE.findFirstMatchIn(text) match {
      case Some(m) => m.group("type")
      case None => ""
    }
  }

  private def parseLine(line: String): Option[LogEvent] = {
    threads += Thread.currentThread().getName
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

    // T=P (Player)
    // T=N (Non-player)
    // T=X (nobody)
    val t = parts(0).charAt(2)

    // R=C (character who collected combat log)
    // R=G (same group as C)
    // R=R (same raid as C)
    // R=O (others)
    // r=X (nobody)
    val r = parts(1).charAt(2)

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
      case 'P' => PC(id, r)
      case 'N' => NPC(id, r)
      case 'X' => NullActorID
      case _ => throw new RuntimeException("Unrecognized entity type: " + s)
    }
  }

  private def getActor(actorID: ActorID, ownerID: ActorID, actorName: Option[String]): Actor = {
    actorsLock.readLock().lock()
    try {
      actors.get(actorID) match {
        case Some(actor) => {
          actorName match {
            case Some(name) => {
              actor.name = name
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
                    println("%s has changed ownership: %s -> %s".format(p, p.owner, owner))
                case _ => println("%s cannot be owned by %s".format(actor, owner))
              }
            }
          }
          actor
        }
        case None => {
          actorsLock.readLock().unlock()
          actorsLock.writeLock().lock()
          try {
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
          }
          finally {
            actorsLock.readLock().lock()
            actorsLock.writeLock().unlock()
          }
          actors(actorID)
        }
      }
    }
    finally {
      actorsLock.readLock().unlock()
    }
  }

  private def getPlayer(id: ActorID): Player = {
   getActor(id, NullActorID, None) match {
     case p: Player => p
     case a => throw new RuntimeException("Actor %s is not player".format(a))
   }
 }
}
