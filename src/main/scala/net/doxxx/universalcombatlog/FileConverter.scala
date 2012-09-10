package net.doxxx.universalcombatlog

import java.io.{FileOutputStream, DataOutputStream, File}

/**
 * Created 12-07-18 7:44 PM by gordon.
 */

object FileConverter {

  def writeEvent(s: DataOutputStream, event: LogEvent) {
    event match {
      case ae: ActorEvent => {
        s.writeLong(ae.time)
        s.writeByte(ae.eventType.id)
        s.writeLong(ae.actor.id.id)
        s.writeLong(ae.target.id.id)
        s.writeLong(ae.spellId)
        s.writeLong(ae.amount)
        s.writeUTF(ae.text)
      }
      case _ => // skip
    }
  }

  def writeFight(s: DataOutputStream, fight: Fight) {
    s.writeUTF(fight.title.getOrElse(""))
    s.writeInt(fight.events.length)
    for (event <- fight.events) {
      writeEvent(s, event)
    }
  }

  def writeFights(s: DataOutputStream, fights: scala.List[Fight]) {
    s.writeBytes("FIT1")
    s.writeInt(fights.length)
    for (fight <- fights) {
      writeFight(s, fight)
    }
  }

  def writeSpellIndex(s: DataOutputStream, events: List[LogEvent]) {
    s.writeBytes("SPL1")
    val spellIndex = EventProcessor.spellIndex(events)
    s.writeInt(spellIndex.size)
    for (id <- spellIndex.keys) {
      s.writeLong(id)
      s.writeUTF(spellIndex(id))
    }
  }

  def writeEntityIndex(s: DataOutputStream, events: List[LogEvent]) {
    s.writeBytes("ENT1")
    val entityIndex = EventProcessor.entityIndex(events)
    s.writeInt(entityIndex.size)
    for (id <- entityIndex.keys) {
      val entity = entityIndex(id)
      var kind: Char = 'X'
      var owner: Option[Actor] = None
      var name = entity.name
      entity match {
        case p: Player => {
          kind = 'P'
        }
        case pp: PlayerPet => {
          kind = 'N'
          owner = Some(pp.owner)
          name = pp._name
        }
        case np: NonPlayer => {
          kind = 'N'
        }
        case _ => // use defaults
      }
      s.writeLong(id)
      s.writeByte(kind)
      s.writeByte(entity.id.rel)
      owner match {
        case Some(o) => s.writeLong(o.id.id)
        case None => s.writeLong(0)
      }
      s.writeUTF(name)
    }
  }

  def writeUniversalCombatLog(file: File, fights: List[Fight]) {
    val s: DataOutputStream = new DataOutputStream(new FileOutputStream(file))
    s.writeBytes("UCL1")
    val allEvents = fights.map(_.events).flatten
    writeEntityIndex(s, allEvents)
    writeSpellIndex(s, allEvents)
    writeFights(s, fights)
    s.close()
  }

  def main(args: Array[String]) {
    var parser: Option[LogParser] = None
    for (arg <- args) {
      arg match {
        case "-rift" => parser = Some(new StartTimeAdjustingParser(new RiftParser))
        case "-wow" => parser = Some(new WoWParser)
        case _ => {
          if (!parser.isDefined) {
            Utils.log("ERROR: Log format must be specified first: -rift or -wow")
            return
          }
          Utils.log("Parsing %s", arg)
          val file = new File((arg))
          val events = parser.get.parse(file)
          val fights = EventProcessor.splitFights(events).filter { f: Fight =>
            f.duration > 5000 && !EventProcessor.sumNonPlayerDamage(f.events).isEmpty
          }
          val outFile = {
            val i = file.getName.lastIndexOf('.')
            if (i < 0) {
              new File(file.getParent, file.getName + ".ucl")
            }
            else {
              new File(file.getParent, file.getName.substring(0, i) + ".ucl")
            }
          }
          Utils.log("Writing %s", outFile.toString)
          writeUniversalCombatLog(outFile, fights)
          Utils.log("Wrote %d fights", fights.size)
        }
      }
    }
  }

  class StartTimeAdjustingParser(delegate: LogParser) extends LogParser {
    def canLoad(f: File) = delegate.canLoad(f)

    def parse(f: File) = {
      val events = delegate.parse(f)
      val startTime = f.lastModified() - events.last.time
      Utils.log("Calculated startTime: %d", startTime)
      EventProcessor.normalizeTimes(events, startTime)
    }
  }
}
