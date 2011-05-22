package org.scalaperf
package time

trait Time {
  def time() = System.nanoTime
  
  def timeDiff(start: Long, end: Long): Long = {
    if (start > end) throw new IllegalStateException("Clock ran backwards: start = " + start + " > end = " + end)
    else (end - start)
  }
}
