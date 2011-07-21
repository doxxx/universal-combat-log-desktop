package net.doxxx.riftcombatparser

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers
import net.doxxx.riftcombatparser.CombatLogParser.{NPC, PC}

class EventProcessorSpec extends WordSpec with ShouldMatchers {
  import EventType._

  "An EventProcessor" when {
    val actor1 = Player(PC(1), "actor1")
    val target1 = NonPlayer(NPC(2), "target1")
    val middleActor1 = Player(PC(3), "middleActor1")
    val middleActor2 = Player(PC(4), "middleActor2")
    val middleTarget1 = NonPlayer(NPC(5), "middleTarget1")
    val middleTarget2 = NonPlayer(NPC(6), "middleTarget2")
    "splitting fights" should {
      val middleFight = List(
        ActorEvent(100, DirectDamage, middleActor1, middleTarget1, "middleSpell1",
          101, 123, "text")
      )
      val middleFightBracketed = List(CombatToggleEvent(1, true)) ::: middleFight ::: List(CombatToggleEvent(200, false))

      "ignore events before first CombatStart" in {
        val start = List(
          ActorEvent(1, DirectDamage, actor1, target1, "spell1", 1, 123, "text")
        )
        val events = start ::: middleFightBracketed
        val fights = EventProcessor.splitFights(events)
        fights should equal (List(SingleFight(middleFight)))
      }

      "treat events before first CombatEnd as a fight" in {
        val start = List(
          ActorEvent(1, DirectDamage, actor1, target1, "spell1", 1, 123, "text")
        )
        val events = start ::: List(CombatToggleEvent(1, false)) ::: middleFightBracketed
        val fights = EventProcessor.splitFights(events)
        fights should equal (List(SingleFight(start), SingleFight(middleFight)))
      }

      "ignore events after last CombatEnd" in {
        val end = List(
          ActorEvent(200, DirectDamage, actor1, target1, "spell1", 1, 123, "text")
        )
        val events = middleFightBracketed ::: end
        val fights = EventProcessor.splitFights(events)
        fights should equal (List(SingleFight(middleFight)))
      }

      "treat events after last CombatStart as a fight" in {
        val end = List(
          ActorEvent(200, DirectDamage, actor1, target1, "spell1", 1, 123, "text")
        )
        val events = middleFightBracketed ::: List(CombatToggleEvent(199, true)) ::: end
        val fights = EventProcessor.splitFights(events)
        fights should equal (List(SingleFight(middleFight), SingleFight(end)))
      }

      "discard events in between fights" in {
        val middleFight2 = List(
          ActorEvent(150, DirectDamage, middleActor2, middleTarget2, "middleSpell2", 102, 123, "text")
        )
        val middleFightBracketed2 = List(CombatToggleEvent(1, true)) ::: middleFight2 ::: List(CombatToggleEvent(1, false))
        val events = middleFightBracketed ::: List(
          ActorEvent(120, DirectDamage, Nobody, Nobody, "discardSpell", 999, 123, "text")
        ) ::: middleFightBracketed2
        val fights = EventProcessor.splitFights(events)
        fights should equal (List(SingleFight(middleFight), SingleFight(middleFight2)))
      }
    }
    "normalizing times" should {
      val events = List(
        CombatToggleEvent(86300, true),
        ActorEvent(86350, DirectDamage, middleActor2, middleTarget2, "middleSpell2", 102, 123, "text"),
        ActorEvent(50, DirectDamage, actor1, target1, "spell1", 1, 123, "text"),
        CombatToggleEvent(100, false)
      )
      val normalized = EventProcessor.normalizeTimes(events)
      "start with time zero" in {
        normalized.head.time should equal (0)
      }
      "handle midnight rollovers" in {
        normalized should equal (List(
          CombatToggleEvent(0, true),
          ActorEvent(50, DirectDamage, middleActor2, middleTarget2, "middleSpell2", 102, 123, "text"),
          ActorEvent(150, DirectDamage, actor1, target1, "spell1", 1, 123, "text"),
          CombatToggleEvent(200, false)
        ))
      }
    }
  }

}
