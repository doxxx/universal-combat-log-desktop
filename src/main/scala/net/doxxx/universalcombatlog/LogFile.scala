package net.doxxx.universalcombatlog

class LogFile(val events: List[LogEvent], val actors: Set[Actor]) {
  lazy val playersAndPets: Set[Actor] = actors.filter {
    case _: Player => true
    case _: PlayerPet => true
    case _ => false
  }

  lazy val fights: List[Fight] = EventProcessor.splitFights(events).filter(_.duration > 5000)

  def normalizeTimes(startTime: Long = 0): LogFile =
    new LogFile(EventProcessor.normalizeTimes(events, startTime), actors)
}
