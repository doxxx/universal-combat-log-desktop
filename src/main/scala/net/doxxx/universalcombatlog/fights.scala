package net.doxxx.universalcombatlog

abstract class Fight {
  def events:List[LogEvent]
  def title:Option[String]
  def startTime:Long
  def endTime:Long
  def duration:Int
  override def toString = "%s (%ds)".format(title.getOrElse("@%d".format(startTime)), duration/1000)
}

case class EmptyFight(time: Long = 0) extends Fight {
  val events = Nil
  val title = None
  val startTime = time
  val endTime = time
  val duration = 0
}

case class SingleFight(events: List[LogEvent]) extends Fight {
  val title = EventProcessor.primaryNPC(events)
  val startTime = if (events.isEmpty) 0 else events.head.time
  val endTime = if (events.isEmpty) 0 else events.last.time
  val duration: Int = (endTime - startTime).toInt
}

case class Fights(fights: List[Fight], title: Option[String] = None) extends Fight {
  lazy val events = fights.map(_.events).flatten
  lazy val startTime = fights.head.startTime
  lazy val endTime = fights.last.endTime
  lazy val duration = fights.map(_.duration).foldLeft(0)(_+_)
}

