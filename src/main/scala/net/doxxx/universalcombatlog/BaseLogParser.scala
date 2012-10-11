package net.doxxx.universalcombatlog

import java.io.File
import collection.mutable
import java.util.concurrent.locks.ReentrantReadWriteLock
import net.doxxx.universalcombatlog.Utils._
import scala.Some
import scalax.io.Resource

abstract class BaseLogParser extends LogParser {

  private var lastFileSize: Long = 0
  private var lastLineNum: Int = 0
  private var lastEvents: List[LogEvent] = Nil

  protected var threads: Set[String] = Set.empty

  private val actorsLock = new ReentrantReadWriteLock()
  private val actors = new mutable.HashMap[EntityID, Entity] {
    override def default(key: EntityID) = Nobody
  }

  def reset() {
    actors.clear()
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

  def parse(file: File): List[LogEvent] = {
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

    lastEvents
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

  def playersAndPets: Set[Entity] = actors.filter {
    case (id: EntityID, player: Player) => true
    case (id: EntityID, playerPet: PlayerPet) => true
    case _ => false
  }.values.toSet

  protected def getActor(actorID: EntityID, ownerID: EntityID, actorName: Option[String]): Entity = {
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
            case NullEntityID => // nothing
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
                val actor: Entity = ownerID match {
                  case NullEntityID => NonPlayer(npc, actorName.getOrElse("$dummy$"))
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

  protected def getPlayer(id: EntityID): Player = {
    getActor(id, NullEntityID, None) match {
      case p: Player => p
      case a => throw new RuntimeException("Entity %s is not player".format(a))
    }
  }
}
