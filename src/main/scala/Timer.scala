/**
  * Created by mmgreiner on 07.11.16.
  */

import java.util.logging._
import scala.util.{Try, Failure, Success}


class Timer (step: Integer = 1000, heapInfo: Boolean = false, log: Logger = null) {

  private val start = System.nanoTime
  private var count = 0

  val Count = () => count

  val elapsed = () => f"${(System.nanoTime.toDouble - start) / 1000000000.0}%6.2f"

  def info (msg: String) {
    var message = f"$count%5.0f ${elapsed()}"
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

  var _progress_count = 0
  var _progress_time = System.nanoTime

}
