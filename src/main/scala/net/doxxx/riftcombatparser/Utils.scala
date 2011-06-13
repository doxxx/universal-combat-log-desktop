package net.doxxx.riftcombatparser

import java.lang.Boolean

object Utils {
  def timeit[U](msg: String)(f: () => U): U = {
    val start = System.currentTimeMillis()
    val r = f()
    val dur = System.currentTimeMillis() - start
    if (Boolean.getBoolean("timeit")) {
      println("%s: %dms".format(msg, dur))
    }
    r
  }
}
