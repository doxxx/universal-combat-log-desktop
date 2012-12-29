package net.doxxx.universalcombatlog

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers

class EventProcessorSpec extends WordSpec with ShouldMatchers {
  import EventTypes._

  "An EventProcessor" when {
    val pc1 = Player(PC(1, 'C'), "pc1")
    val pc2 = Player(PC(2, 'G'), "pc2")
    val pc3 = Player(PC(3, 'G'), "cp3")
    val npc1 = NonPlayer(NPC(4, 'O'), "npc1")
    val npc2 = NonPlayer(NPC(5, 'O'), "npc2")
    val npc3 = NonPlayer(NPC(6, 'O'), "npc3")

    "splitting fights" should {
      "end a fight after 5 seconds of inactivity" in {
        val fight1 = List(
          ActorEvent(0, DirectDamage, pc1, npc1, "spell1", 1, 123, "text"),
          ActorEvent(1000, DirectDamage, pc1, npc1, "spell1", 1, 123, "text"),
          ActorEvent(2000, DirectDamage, pc1, npc1, "spell1", 1, 123, "text")
        )
        val fight2 = List(
          ActorEvent(7000, DirectDamage, pc1, npc1, "spell1", 1, 123, "text")
        )
        val events = fight1 ::: fight2
        val fights = EventProcessor.splitFights(events)
        fights should equal (List(SingleFight(fight1), SingleFight(fight2)))
      }

      "end a fight after all NPCs have died" in {
        val fight1 = List(
          ActorEvent(0, DirectDamage, pc1, npc1, "spell1", 1, 123, "text"),
          ActorEvent(1000, DirectDamage, pc2, npc2, "spell1", 1, 123, "text"),
          ActorEvent(2000, DirectDamage, pc3, npc3, "spell1", 1, 123, "text"),
          ActorEvent(3000, Died, npc1, Nobody, "", 0, 0, "text"),
          ActorEvent(4000, Died, npc2, Nobody, "", 0, 0, "text"),
          ActorEvent(5000, Died, npc3, Nobody, "", 0, 0, "text")
        )
        val fight2 = List(
          ActorEvent(6000, DirectDamage, pc1, npc1, "spell1", 1, 123, "text")
        )
        val events = fight1 ::: fight2
        val fights = EventProcessor.splitFights(events)
        fights should equal (List(SingleFight(fight1), SingleFight(fight2)))
      }

      "end a fight after all PCs have died" in {
        val fight1 = List(
          ActorEvent(0, DirectDamage, pc1, npc1, "spell1", 1, 123, "text"),
          ActorEvent(1000, DirectDamage, pc2, npc1, "spell1", 1, 123, "text"),
          ActorEvent(2000, DirectDamage, pc3, npc1, "spell1", 1, 123, "text"),
          ActorEvent(3000, Died, pc1, Nobody, "", 0, 0, "text"),
          ActorEvent(4000, Died, pc2, Nobody, "", 0, 0, "text"),
          ActorEvent(5000, Died, pc3, Nobody, "", 0, 0, "text")
        )
        val fight2 = List(
          ActorEvent(6000, DirectDamage, pc1, npc1, "spell1", 1, 123, "text")
        )
        val events = fight1 ::: fight2
        val fights = EventProcessor.splitFights(events)
        fights should equal (List(SingleFight(fight1), SingleFight(fight2)))
      }

      "collect remaining events as a fight" in {
        val fight1 = List(
          ActorEvent(0, DirectDamage, pc1, npc1, "spell1", 1, 123, "text"),
          ActorEvent(1000, DirectDamage, pc1, npc1, "spell1", 1, 123, "text"),
          ActorEvent(2000, DirectDamage, pc1, npc1, "spell1", 1, 123, "text")
        )
        val events = fight1
        val fights = EventProcessor.splitFights(events)
        fights should equal (List(SingleFight(fight1)))
      }
    }

    "normalizing times" should {
      val events = List(
        CombatToggleEvent(86300000, inCombat = true),
        ActorEvent(86350000, DirectDamage, pc3, npc3, "middleSpell2", 102, 123, "text"),
        ActorEvent(50000, DirectDamage, pc1, npc1, "spell1", 1, 123, "text"),
        CombatToggleEvent(100000, inCombat = false)
      )
      val normalized = EventProcessor.normalizeTimes(events, 0)
      "start with time zero" in {
        normalized.head.time should equal (0)
      }
      
      "handle midnight rollovers" in {
        normalized should equal (List(
          CombatToggleEvent(0, inCombat = true),
          ActorEvent(50000, DirectDamage, pc3, npc3, "middleSpell2", 102, 123, "text"),
          ActorEvent(150000, DirectDamage, pc1, npc1, "spell1", 1, 123, "text"),
          CombatToggleEvent(200000, inCombat = false)
        ))
      }
    }
  }

}
