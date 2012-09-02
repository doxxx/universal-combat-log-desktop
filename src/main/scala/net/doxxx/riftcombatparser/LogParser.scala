package net.doxxx.riftcombatparser

import java.io.File
import collection.mutable
import java.util.concurrent.locks.ReentrantReadWriteLock

abstract class LogParser {

  private val actorsLock = new ReentrantReadWriteLock()
  private val actors = new mutable.HashMap[ActorID, Actor] {
    override def default(key: ActorID) = Nobody
  }

  def reset() {
    actors.clear()
  }

  def parse(f: File): List[LogEvent]

  def playersAndPets: Set[Actor] = actors.filter {
    case (id: ActorID, player: Player) => true
    case (id: ActorID, playerPet: PlayerPet) => true
    case _ => false
  }.values.toSet

  protected def getActor(actorID: ActorID, ownerID: ActorID, actorName: Option[String]): Actor = {
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

  protected def getPlayer(id: ActorID): Player = {
    getActor(id, NullActorID, None) match {
      case p: Player => p
      case a => throw new RuntimeException("Actor %s is not player".format(a))
    }
  }
}
