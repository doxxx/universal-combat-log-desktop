package net.doxxx.universalcombatlog

class LogFile(val events: List[LogEvent], val entities: Set[Entity]) {
  lazy val playersAndPets: Set[Entity] = entities.filter {
    case _: Player => true
    case _: PlayerPet => true
    case _ => false
  }

  lazy val fights: List[Fight] = EventProcessor.splitFights(events).filter(_.duration > 5000)

  def normalizeTimes(startTime: Long = 0): LogFile =
    new LogFile(EventProcessor.normalizeTimes(events, startTime), entities)
}
