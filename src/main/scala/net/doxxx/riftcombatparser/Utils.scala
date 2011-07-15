package net.doxxx.riftcombatparser

import java.lang.Boolean
import java.util.Calendar
import java.text.DateFormat

object Utils {
  private val LoggingDateFormat = DateFormat.getDateTimeInstance(DateFormat.SHORT, DateFormat.LONG)

  private def formattedDate = {
    LoggingDateFormat.format(Calendar.getInstance.getTime)
  }

  def log(msg: String, args: Any*) {
    val a = formattedDate :: args.toList
    printf("[%s] %s: " + msg + "\n", Thread.currentThread().getName :: a: _*)
  }

  def timeit[U](msg: String)(f: () => U): U = {
    val start = System.currentTimeMillis()
    val r = f()
    val dur = System.currentTimeMillis() - start
    if (Boolean.getBoolean("timeit")) {
      log("%s: %dms".format(msg, dur))
    }
    r
  }
}
