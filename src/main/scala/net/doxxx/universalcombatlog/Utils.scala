package net.doxxx.universalcombatlog

import java.lang.Boolean
import java.text.DateFormat
import java.util

object Utils {
  private val LoggingDateFormat = DateFormat.getDateTimeInstance(DateFormat.SHORT, DateFormat.LONG)
  private val DebugMode = Boolean.getBoolean("debug")

  private def formattedDate = {
    LoggingDateFormat.format(util.Calendar.getInstance.getTime)
  }

  def log(msg: String, args: Any*) {
    val formattedMsg = msg.format(args: _*)
    printf("%s [%s] %s\n", formattedDate, Thread.currentThread.getName, formattedMsg)
  }

  def debuglog(msg: String, args: Any*) {
    if (DebugMode) log(msg, args: _*)
  }

  def timeit[U](msg: String)(f: => U): U = {
    val start = System.currentTimeMillis()
    val r = f
    val dur = System.currentTimeMillis() - start
    if (Boolean.getBoolean("timeit")) {
      log("%s: %dms".format(msg, dur))
    }
    r
  }
}
