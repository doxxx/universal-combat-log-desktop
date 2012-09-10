package net.doxxx.universalcombatlog

import java.io.File

trait LogParser {
  def canLoad(f: File): Boolean
  def parse(file: File): List[LogEvent]
  def playersAndPets: Set[Actor]
}

object LogParser {
  val parallelize = !java.lang.Boolean.getBoolean("nopar")
  val knownParsers = List[LogParser](new RiftParser, new WoWParser)
  def detectFormat(f: File): Option[LogParser] = {
    knownParsers.find(p => p.canLoad(f))
  }
}