package net.doxxx.universalcombatlog.parser

import collection.mutable
import java.io.File
import java.util.concurrent.locks.ReentrantReadWriteLock
import net.doxxx.universalcombatlog.Utils._
import net.doxxx.universalcombatlog.LogFile
import scalax.io.Resource
import net.doxxx.universalcombatlog.spells._

abstract class BaseLogParser extends LogParser {

  private var lastFileSize: Long = 0
  private var lastLineNum: Int = 0
  private var lastEvents: List[LogEvent] = Nil

  protected var threads: Set[String] = Set.empty

  private val entitiesLock = new ReentrantReadWriteLock()
  private val entities = new mutable.HashMap[EntityID, Entity] {
    override def default(key: EntityID) = Nobody
  }
  private val spellsLock = new ReentrantReadWriteLock()
  private val spells = new mutable.HashMap[Long, Spell]

  def reset() {
    entities.clear()
    threads = Set.empty
    lastLineNum = 0
    lastFileSize = 0
    lastEvents = Nil
  }

  def canLoad(f: File): Boolean = {
    Resource.fromFile(f).lines().headOption match {
      case Some(line) => parseLine(line).isDefined
      case _ => false
    }
  }

  def parse(file: File): LogFile = {
    if (file.length() < lastFileSize) {
      log("File size smaller than last position; resetting")
      reset()
    }

    lastFileSize = file.length()
    if (lastLineNum > 0) {
      log("Skipping %d lines...", lastLineNum)
    }

    val lines = timeit("Loading lines") {
      Resource.fromFile(file).lines().drop(lastLineNum).toList
    }

    log("Loaded %d lines", lines.size)

    lastLineNum += lines.size
    threads = Set.empty

    val newEvents = parseLines(lines)

    log("Parsed %d new events using %d threads", newEvents.length, threads.size)

    lastEvents = lastEvents ::: newEvents

    log("Total events parsed: %d", lastEvents.size)

    new LogFile(file.getName, lastEvents, entities.values.toSet, spells.values.toSet)
  }

  private def parseLines(lines: Traversable[String]): List[LogEvent] = {
    timeit("Parsing lines") {
      if (LogParser.parallelize) {
        log("Parsing in parallel")
        lines.par.map(parseLine).flatten.toList
      }
      else {
        lines.map(parseLine).flatten.toList
      }
    }
  }

  protected def parseLine(line: String): Option[LogEvent]

  def playersAndPets: Set[Entity] = entities.filter {
    case (id: EntityID, player: Player) => true
    case (id: EntityID, playerPet: PlayerPet) => true
    case _ => false
  }.values.toSet

  protected def getEntity(id: EntityID, ownerID: EntityID, name: Option[String]): Entity = {
    entitiesLock.readLock().lock()
    try {
      entities.get(id) match {
        case Some(entity) => {
          entity.name = name.getOrElse(entity.name)
          ownerID match {
            case NullEntityID => // nothing
            case _ => {
              val owner = entities(ownerID)
              entity match {
                case p: PlayerPet =>
                  if (p.owner.id != owner.id)
                    println("%s has changed ownership: %s -> %s".format(p, p.owner, owner))
                case _ => println("%s cannot be owned by %s".format(entity, owner))
              }
            }
          }
          entity
        }
        case None => {
          entitiesLock.readLock().unlock()
          entitiesLock.writeLock().lock()
          try {
            id match {
              case pc: PC => {
                entities += id -> Player(pc, name.getOrElse("$dummy$"))
              }
              case npc: NPC => {
                val entity: Entity = ownerID match {
                  case NullEntityID => NonPlayer(npc, name.getOrElse("$dummy$"))
                  case _ => PlayerPet(npc, name.getOrElse("$dummy$"), getPlayer(ownerID))
                }
                entities += id -> entity
              }
              case _ => // nothing
            }
          }
          finally {
            entitiesLock.readLock().lock()
            entitiesLock.writeLock().unlock()
          }
          entities(id)
        }
      }
    }
    finally {
      entitiesLock.readLock().unlock()
    }
  }

  protected def getPlayer(id: EntityID): Player = {
    getEntity(id, NullEntityID, None) match {
      case p: Player => p
      case a => throw new RuntimeException("Entity %s is not player".format(a))
    }
  }

  protected def getSpell(id: Long, name: String): Spell = {
    spellsLock.readLock.lock()
    try {
      spells.get(id) match {
        case Some(spell) => {
          if (spell.name != name) {
            log("Mismatched name for spell ID 0x%016X: old name=%s, new name=%s", id, spell.name, name)
          }
          spell
        }
        case None => {
          spellsLock.readLock.unlock()
          spellsLock.writeLock.lock()
          try {
            val spell = Spell(id, name)
            spells.update(id, spell)
            spell
          }
          finally {
            spellsLock.writeLock.unlock()
            spellsLock.readLock.lock()
          }
        }
      }
    }
    finally {
      spellsLock.readLock.unlock()
    }
  }
}
