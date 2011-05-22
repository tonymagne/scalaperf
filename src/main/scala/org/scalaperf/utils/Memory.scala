package org.scalaperf
package utils

import scala.annotation.tailrec
import java.lang.management.ManagementFactory

case class MemorySnapshot(total: Long, max: Long, free: Long) {
  require(total > 0, "Total memory must be greater than 0")
  require(max > 0, "Max memory must be greater than 0")
  require(free >= 0, "Free memory must be greater or equal to 0")
  
  val used = total - free
  val available = max - used
  val ratio: Double = available / max
}

object Memory {
  val maxRestoreAttempt = 100
  val pauseTime: Long = 1
  
  def restore = tryRestore(Long.MaxValue, used, maxRestoreAttempt) match {
    case None => None
    case Some(n) => Some(maxRestoreAttempt - n)
  }
  
  @tailrec
  private def tryRestore(attempt: (Long, Long, Int)): Option[Int] = attempt match {
    case (previous, current, 0) => None
    case (previous, current, n) if (current >= previous && !hasPendingFinalization) => Some(n)
    case (previous, current, n) => clean; tryRestore(current, used, n - 1)
  }
  
  def used = Runtime.getRuntime.totalMemory - Runtime.getRuntime.freeMemory
  
  def snapshot = MemorySnapshot(Runtime.getRuntime.totalMemory, Runtime.getRuntime.maxMemory, Runtime.getRuntime.freeMemory)
  
  private def hasPendingFinalization = ManagementFactory.getMemoryMXBean.getObjectPendingFinalizationCount > 0
 
  private def clean: Unit = {
    System.runFinalization
    System.gc
  }

}

