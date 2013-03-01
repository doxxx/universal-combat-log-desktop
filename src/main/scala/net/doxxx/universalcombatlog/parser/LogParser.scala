package net.doxxx.universalcombatlog.parser

import java.io.File
import net.doxxx.universalcombatlog.LogFile

trait LogParser {
  def canLoad(f: File): Boolean
  def parse(file: File): LogFile
  def playersAndPets: Set[Entity]
}

object LogParser {
  val parallelize = !java.lang.Boolean.getBoolean("nopar")
  val knownParsers = List[LogParser](new RiftParser, new WoWParser)
  def detectFormat(f: File): Option[LogParser] = {
    knownParsers.find(p => p.canLoad(f))
  }
}
