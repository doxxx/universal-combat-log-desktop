package net.doxxx.universalcombatlog

import util.matching.Regex

final class RiftParser extends BaseLogParser {
  private val CombatToggleRE = new Regex("([0-9][0-9]:[0-9][0-9]:[0-9][0-9]) Combat (Begin|End)", "time", "toggle")
  private val DataRE =
    new Regex("([0-9]+) , (T=.+) , (T=.+) , (T=.+) , (T=.+) , (.*?) , (.*?) , (-?[0-9]*) , ([0-9]*) , (.*?)",
      "actorInfo", "targetInfo", "actorOwnerInfo", "targetOwnerInfo", "eventType", "actorName", "targetName",
      "amount", "spellId", "spell")
  private val LineRE = new Regex("([0-9][0-9]:[0-9][0-9]:[0-9][0-9]): \\( (.+?) \\) (.+)", "time", "data", "text")

  private val OverhealRE = new Regex("([0-9]+) overheal", "amount")
  private val OverkillRE = new Regex("([0-9]+) overkill", "amount")
  private val AbsorbedRE = new Regex("([0-9]+) absorbed", "amount")
  private val BlockedRE = new Regex("([0-9]+) blocked", "amount")
  private val DamageTypeRE = new Regex("[0-9]+ (.+) damage", "type")

  protected def parseLine(line: String): Option[LogEvent] = {
    threads += Thread.currentThread().getName
    line match {
      case CombatToggleRE(time, toggle) => Some(CombatToggleEvent(parseTime(time), parseCombatToggle(toggle)))
      case LineRE(time, data, text) => parseActorEvent(time, data, text)
      case _ => None
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

  def parseOverAmount(eventType: EventTypes.Value, text: String): Int = {
    if (EventTypes.DamageTypes.contains(eventType)) {
      extractOverkill(text)
    }
    else if (EventTypes.HealTypes.contains(eventType)) {
      extractOverheal(text)
    }
    else {
      0
    }
  }

  private def parseActorEvent(time: String, data: String, text: String): Option[CombatEvent] = {
    data match {
      case DataRE(eventTypeID, actorInfo, targetInfo, actorOwnerInfo, targetOwnerInfo, actorName,
      targetName, amountText, spellId, spell) => {
        val eventType = EventTypes(eventTypeID.toInt)
        val actor = getEntity(parseEntity(actorInfo), parseEntity(actorOwnerInfo), Some(actorName))
        val target = getEntity(parseEntity(targetInfo), parseEntity(targetOwnerInfo), Some(targetName))
        val overAmount = parseOverAmount(eventType, text)
        val amount =
          if (EventTypes.DamageTypes.contains(eventType))
            amountText.toInt - overAmount // damage amounts include overkill
          else
            amountText.toInt // heal amounts don't include overheal
        Some(CombatEvent(parseTime(time), eventType, actor, target, spell, spellId.toLong,
          extractDamageType(text), amount, overAmount, text))
      }
      case _ => {
        println("Unrecognized data string: " + data)
        None
      }
    }
  }

  private def parseEntity(s: String): EntityID = {
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
      case 'X' => NullEntityID
      case _ => throw new RuntimeException("Unrecognized entity type: " + s)
    }
  }

  private def extractOverheal(text: String): Int = {
    OverhealRE.findFirstMatchIn(text) match {
      case Some(m) => m.group("amount").toInt
      case None => 0
    }
  }

  private def extractOverkill(text: String): Int = {
    OverkillRE.findFirstMatchIn(text) match {
      case Some(m) => m.group("amount").toInt
      case None => 0
    }
  }

  private def extractAbsorbed(text: String): Int = {
    AbsorbedRE.findFirstMatchIn(text) match {
      case Some(m) => m.group("amount").toInt
      case None => 0
    }
  }

  private def extractBlocked(text: String): Int = {
    BlockedRE.findFirstMatchIn(text) match {
      case Some(m) => m.group("amount").toInt
      case None => 0
    }
  }

  private def extractDamageType(text: String): String = {
    DamageTypeRE.findFirstMatchIn(text) match {
      case Some(m) => m.group("type")
      case None => ""
    }
  }

}
