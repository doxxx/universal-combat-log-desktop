package net.doxxx.riftcombatparser

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers

class EventProcessorSpec extends WordSpec with ShouldMatchers {
  import EventType._

  "An EventProcessor" when {
    "splitting fights" should {
      val middleFight = List(
        ActorEvent(100, DirectDamage, "middleActor1", "middleTarget1", "middleSpell1", 101, 123, "text")
      )
      val middleFightBracketed = List(CombatToggleEvent(1, true)) ::: middleFight ::: List(CombatToggleEvent(1, false))

      "ignore events + CombatStart at beginning" in {
        val start = List(
          ActorEvent(1, DirectDamage, "actor1", "target1", "spell1", 1, 123, "text")
        )
        val events = start ::: middleFightBracketed
        val fights = EventProcessor.splitFights(events)
        fights should equal (List(SingleFight(middleFight)))
      }

      "treat events + CombatEnd at beginning as a fight" in {
        val start = List(
          ActorEvent(1, DirectDamage, "actor1", "target1", "spell1", 1, 123, "text")
        )
        val events = start ::: List(CombatToggleEvent(1, false)) ::: middleFightBracketed
        val fights = EventProcessor.splitFights(events)
        fights should equal (List(SingleFight(start), SingleFight(middleFight)))
      }

      "ignore CombatEnd + events at end" in {
        val end = List(
          ActorEvent(200, DirectDamage, "actor1", "target1", "spell1", 1, 123, "text")
        )
        val events = middleFightBracketed ::: end
        val fights = EventProcessor.splitFights(events)
        fights should equal (List(SingleFight(middleFight)))
      }

      "treat CombatStart + events at end as a fight" in {
        val end = List(
          ActorEvent(200, DirectDamage, "actor1", "target1", "spell1", 1, 123, "text")
        )
        val events = middleFightBracketed ::: List(CombatToggleEvent(199, true)) ::: end
        val fights = EventProcessor.splitFights(events)
        fights should equal (List(SingleFight(middleFight), SingleFight(end)))
      }

      "discard events in between fights" in {
        val middleFight2 = List(
          ActorEvent(150, DirectDamage, "middleActor2", "middleTarget2", "middleSpell2", 102, 123, "text")
        )
        val middleFightBracketed2 = List(CombatToggleEvent(1, true)) ::: middleFight2 ::: List(CombatToggleEvent(1, false))
        val events = middleFightBracketed ::: List(
          ActorEvent(120, DirectDamage, "discardActor", "discartTarget", "discardSpell", 999, 123, "text")
        ) ::: middleFightBracketed2
        val fights = EventProcessor.splitFights(events)
        fights should equal (List(SingleFight(middleFight), SingleFight(middleFight2)))
      }
    }
    "normalizing times" should {
      val events = List(
        CombatToggleEvent(86300, true),
        ActorEvent(86350, DirectDamage, "middleActor2", "middleTarget2", "middleSpell2", 102, 123, "text"),
        ActorEvent(50, DirectDamage, "actor1", "target1", "spell1", 1, 123, "text"),
        CombatToggleEvent(100, false)
      )
      val normalized = EventProcessor.normalizeTimes(events)
      "start with time zero" in {
        normalized.head.time should equal (0)
      }
      "handle midnight rollovers" in {
        normalized should equal (List(
          CombatToggleEvent(0, true),
          ActorEvent(50, DirectDamage, "middleActor2", "middleTarget2", "middleSpell2", 102, 123, "text"),
          ActorEvent(150, DirectDamage, "actor1", "target1", "spell1", 1, 123, "text"),
          CombatToggleEvent(200, false)
        ))
      }
    }
  }

}
