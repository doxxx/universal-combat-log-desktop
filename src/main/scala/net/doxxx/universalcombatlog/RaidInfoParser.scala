package net.doxxx.universalcombatlog

import xml.XML
import java.io.Reader


/**
 * Parses raid.xml dumped from RIFT.
 */
class RaidInfoParser(input: Reader) {
  def parse(): RaidInfo = {
    RaidInfo(XML.load(input) \ "Parties" \ "Party" \ "Members" \ "Member" \ "Name" map { n => RaidMember(n.text) } toList)
  }
}

case class RaidMember(name: String)
case class RaidInfo(members: List[RaidMember])
