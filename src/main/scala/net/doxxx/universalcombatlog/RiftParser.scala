package net.doxxx.universalcombatlog

import util.matching.Regex
import java.io._
import java.lang.{Boolean, RuntimeException}
import collection.mutable

final class RiftParser extends LogParser {
  import Utils._

  private val parallelize = !Boolean.getBoolean("nopar")

  private val CombatToggleRE = new Regex("([0-9][0-9]:[0-9][0-9]:[0-9][0-9]) Combat (Begin|End)", "time", "toggle")
  private val DataRE =
    new Regex("([0-9]+) , (T=.+) , (T=.+) , (T=.+) , (T=.+) , (.*?) , (.*?) , (-?[0-9]*) , ([0-9]*) , (.*?)",
              "actorInfo", "targetInfo", "actorOwnerInfo", "targetOwnerInfo", "eventType", "actorName", "targetName",
              "amount", "spellId", "spell")
  private val LineRE = new Regex("([0-9][0-9]:[0-9][0-9]:[0-9][0-9]): \\( (.+?) \\) (.+)", "time", "data", "text")

  private var lastFilePos: Long = 0
  private var lastEvents: List[LogEvent] = Nil
  private var threads: Set[String] = Set.empty

  override def reset() {
    super.reset()
    lastFilePos = 0
    lastEvents = Nil
  }

  override def parse(file: File): List[LogEvent] = {
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
      parseLines(lines.toList)
    }

    log("Parsed %d events using %d threads", newEvents.length, threads.size)

    lastEvents = lastEvents ::: newEvents

    lastEvents
  }

  private def parseLines(lines: List[String]): List[LogEvent] = {
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
    (parts(0).toLong * 60 * 60 + parts(1).toLong * 60 + parts(2).toLong) * 1000
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
    // 9223372041715776949 (when T=N, most significant bit is set)
    val id = BigInt(parts(2)).toLong

    t match {
      case 'P' => PC(id, r)
      case 'N' => NPC(id, r)
      case 'X' => NullActorID
      case _ => throw new RuntimeException("Unrecognized entity type: " + s)
    }
  }
}

object RiftParser {
  private val OverhealRE = new Regex("([0-9]+) overheal", "amount")
  private val OverkillRE = new Regex("([0-9]+) overkill", "amount")
  private val AbsorbedRE = new Regex("([0-9]+) absorbed", "amount")
  private val DamageTypeRE = new Regex("[0-9]+ (.+) damage", "type")

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

}
