package net.doxxx.universalcombatlog

case class Summary(start: Long = Long.MaxValue, end: Long = 0, damageIn: Int = 0, dpsIn: Int = 0,
                   damageOut: Int = 0, dpsOut: Int = 0,
                   healingIn: Int = 0, hpsIn: Int = 0,
                   healingOut: Int = 0, hpsOut: Int = 0,
                   overhealing: Int = 0,
                   deaths: Int = 0,
                   duration: Int = 0) {
  def updateTimes(time: Long) = copy(start = scala.math.min(start, time), end = scala.math.max(end, time))
  def addDamageIn(amount: Int) = copy(damageIn = damageIn + amount)
  def addDamageOut(amount: Int) = copy(damageOut = damageOut + amount)
  def addHealingIn(amount: Int) = copy(healingIn = healingIn + amount)
  def addHealingOut(amount: Int) = copy(healingOut = healingOut + amount)
  def addOverhealing(amount: Int) = copy(overhealing = overhealing + amount)
  def addDeath() = copy(deaths = deaths + 1, duration = duration + (end - start).toInt, start = Long.MaxValue, end = 0)
  def combatTime: Int = {
    duration + {
      if (start == Long.MaxValue && end == 0)
        0
      else
        (end - start).toInt
    }
  }
  def calculatePerSecond(fightDuration: Int) = {
    val d = (if (EventProcessor.useActorCombatTime) combatTime else fightDuration) / 1000
    if (d > 0)
      copy(dpsIn = damageIn / d, dpsOut = damageOut / d,
        hpsIn = healingIn / d, hpsOut = healingOut / d)
    else
      this
  }
}
