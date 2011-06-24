package net.doxxx.riftcombatparser

import io.Source
import collection.immutable.TreeMap
import collection.mutable.HashMap

object InspectLog {

  def mapEventTypes(events: List[LogEvent]) {
    var m = TreeMap[EventType.Value, String]()
    for (event <- events) {
      event match {
        case ae: ActorEvent => m += ae.eventType -> ae.text
        case _ =>
      }
    }
    for ((id, text) <- m) {
      println("%s\t%s".format(id.toString, text))
    }
  }

  def mapEntityRValuesToIDs(events: List[LogEvent]) {
    val m = new HashMap[Char, Set[String]]() {
      override def default(key: Char) = Set.empty
    }
    for (event <- events) {
      event match {
        case ae: ActorEvent => {
          ae.actorInfo match {
            case PC(r, id) => m += r -> (m(r) + ("P" + id.toString))
            case NPC(r, id) => m += r -> (m(r) + ("N" + id.toString))
            case _ => // nothing
          }
          ae.targetInfo match {
            case PC(r, id) => m += r -> (m(r) + ("P" + id.toString))
            case NPC(r, id) => m += r -> (m(r) + ("N" + id.toString))
            case _ => // nothing
          }
          ae.actorOwnerInfo match {
            case PC(r, id) => m += r -> (m(r) + ("P" + id.toString))
            case NPC(r, id) => m += r -> (m(r) + ("N" + id.toString))
            case _ => // nothing
          }
          ae.targetOwnerInfo match {
            case PC(r, id) => m += r -> (m(r) + ("P" + id.toString))
            case NPC(r, id) => m += r -> (m(r) + ("N" + id.toString))
            case _ => // nothing
          }

        }
        case _ => // do nothing
      }
    }
    for ((r, ids) <- m) {
      println("%c => [%d] %s".format(r, ids.size, ids.mkString(", ")))
    }
  }

  def mapEntitiesToOwners(events: List[LogEvent]) {
    val m = new HashMap[Entity, Set[Entity]] {
      override def default(key: Entity) = Set.empty
    }
    for(event <- events) {
      event match {
        case ae: ActorEvent => {
          ae.actorOwnerInfo match {
            case Nobody => // nothing
            case owner => m += owner -> (m(owner) + ae.actorInfo)
          }
          ae.targetOwnerInfo match {
            case Nobody => // nothing
            case owner => m += owner -> (m(owner) + ae.targetInfo)
          }
        }
        case _ => // do nothing
      }
    }
    for ((owner, pets) <- m) {
      println("%s => [%d] %s".format(owner.toString, pets.size, pets.mkString(", ")))
    }
  }

  def main(args: Array[String]) {
    for (arg <- args) {
      val events = CombatLogParser.parse(Source.fromFile(arg))
      //mapEventTypes(events)
      //mapEntityRValuesToIDs(events)
      mapEntitiesToOwners(events)
    }
  }

}
