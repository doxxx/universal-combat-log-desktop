package net.doxxx.universalcombatlog

import java.io.{OutputStream, FileOutputStream, DataOutputStream, File}
import net.doxxx.universalcombatlog.parser._

/**
 * Created 12-07-18 7:44 PM by gordon.
 */

object FileConverter {

  def writeEvent(s: DataOutputStream, event: LogEvent) {
    event match {
      case ce: CombatEvent => {
        s.writeLong(ce.time)
        s.writeByte(ce.eventType.id)
        s.writeLong(ce.actor.id.id)
        s.writeLong(ce.target.id.id)
        s.writeLong(ce.spell.id)
        s.writeLong(ce.amount)
        s.writeUTF(ce.text)
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
      val spell = spellIndex(id)
      s.writeLong(id)
      s.writeUTF(spell.name)
    }
  }

  def writeEntityIndex(s: DataOutputStream, events: List[LogEvent]) {
    s.writeBytes("ENT1")
    val entityIndex = EventProcessor.entityIndex(events)
    s.writeInt(entityIndex.size)
    for (id <- entityIndex.keys) {
      val entity = entityIndex(id)
      var kind: Char = 'X'
      var owner: Option[Entity] = None
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

  def writeUniversalCombatLog(stream: OutputStream, fights: scala.List[Fight]) {
    val s: DataOutputStream = new DataOutputStream(stream)
    s.writeBytes("UCL1")
    val allEvents = fights.map(_.events).flatten
    writeEntityIndex(s, allEvents)
    writeSpellIndex(s, allEvents)
    writeFights(s, fights)
    s.close()
  }

  def writeUniversalCombatLog(file: File, fights: List[Fight]) {
    writeUniversalCombatLog(new FileOutputStream(file), fights)
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
          val logFile = parser.get.parse(file)
          val fights = EventProcessor.splitFights(logFile.events).filter { f: Fight =>
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
      val logFile = delegate.parse(f)
      val startTime = f.lastModified() - logFile.events.last.time
      Utils.log("Calculated startTime: %d", startTime)
      new LogFile(f.getName, EventProcessor.normalizeTimes(logFile.events, startTime), logFile.entities, logFile.spells)
    }

    def playersAndPets = delegate.playersAndPets
  }
}
