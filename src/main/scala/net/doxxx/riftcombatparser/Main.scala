package net.doxxx.riftcombatparser

import io.Source

object Main {
  def main(args: Array[String]) {
    for (arg <- args) {
      parse(arg)
    }
  }

  def parse(filename: String) {
    val events = new Parser(Source.fromFile(filename)).parse()
    for (event <- events) {
      println(event)
    }
  }
}
