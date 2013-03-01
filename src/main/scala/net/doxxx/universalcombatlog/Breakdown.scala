package net.doxxx.universalcombatlog

case class Breakdown(amount: Int = 0, hits: Int = 0, misses: Int = 0, crits: Int = 0, damageTypes: Set[String] = Set.empty, percent: Int = 0) {
  def addAmount(newAmount: Int) = copy(amount = amount + newAmount)
  def addHit() = copy(hits = hits + 1)
  def addMiss() = copy(misses = misses + 1)
  def addCrit() = copy(crits = crits + 1)
  def addDamageType(damageType: String) = copy(damageTypes = damageTypes + damageType)
  def setPercentOfTotal(total: Int) =
    copy(percent = scala.math.round(amount.toDouble / total.toDouble * 100.0).toInt)
}
