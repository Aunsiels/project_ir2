/**
  * Created by mmgreiner on 07.11.16.
  */

import java.util.Calendar
import java.util.logging._

import scala.util.{Failure, Success, Try}


case class Timer (step: Integer = 1000, heapInfo: Boolean = false, log: Logger = null) {

  private val start = System.nanoTime
  private var count = 0

  val Count = () => count

  def elapsed() = f"${(System.nanoTime.toDouble - start) / 1000000000.0}%6.2f"

  def info (msg: String) {
    var message = f"$count%5d ${elapsed()}"
    if (heapInfo)
      message += f" ${Timer.freeMB()}%6.2f MB"
    message = s"$message: $msg"
    if (log == null) println(message) else log.info(message)
  }

  val progress = (message: String) => {
    if (count % step == 0)
      info(message)
    count += 1
  }
}

object Timer {
  val runtime = Runtime.getRuntime

  val freeMB = () => runtime.freeMemory().toDouble / 1000000.0

  val totalMB = () => runtime.totalMemory().toDouble / 1000000.0

  val availMB = () => totalMB() - freeMB()

  var _progress_count = 0
  var _progress_time = System.nanoTime

  // simplify format of logging
  System.setProperty("java.util.logging.SimpleFormatter.format", "%1$tT %4$s %2$s: %5$s%6$s%n")

  val log = Logger.getLogger("Timer")

  val user = System.getProperty("user.name")

  val now = () => {
    val now = java.util.Calendar.getInstance()
    now.get(Calendar.YEAR) + "-" + now.get(Calendar.MONTH) + "-" + now.get(Calendar.DAY_OF_MONTH) +
      "-" + now.get(Calendar.HOUR) + ":" + now.get(Calendar.MINUTE)
  }


}
