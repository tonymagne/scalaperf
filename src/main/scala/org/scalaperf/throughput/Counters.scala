package org.scalaperf
package throughput

import scala.actors.Actor
import scala.actors.Actor._
import time.Time
import fub.{Fub1, Fub2, Fub3}

case object Hit
case class Batch(nb: Int)
case object Reset
case object Stop

private[throughput] class ThroughputCounter extends Actor {
  val oneSecond = 1e9
  private var acc = 0
  private var startTime: Long = 0L
  private var endTime: Long = 0L
  
  def act() {
    loop { react {
      case Hit => {
        if (acc == 0) startTime = System.nanoTime
        endTime = System.nanoTime
        acc += 1
      }
      case Batch(nb) => {
        if (acc == 0) startTime = System.nanoTime
        endTime = System.nanoTime
        acc += nb
      }
      case Reset => {
        acc = 0
      }
      case Stop => exit()
    }}
  }
  
  def get: Double = if (acc > 0) (acc * oneSecond) / (endTime - startTime) else 0
  
  def count = acc
}

trait Counter extends Time {
  val counter = new ThroughputCounter
  def exec(n: Long): Unit
  def measure(n: Long): Long
  
  protected def loop(n: Long)(proc: => Unit): Unit = {
    var i = n
    while (i > 0) {
      proc
      i = i - 1
    }
  }
  
  def accumulate(result: Any): Unit = result match {
    case aSeq: Seq[_] => counter ! Batch(aSeq.length)
    case _ => counter ! Hit
  }
}

class Counter1[A, B](fub: Fub1[A, B]) extends Counter {
  def exec(n: Long): Unit = loop(n) {
    val result = fub.function(fub.sample)
    accumulate(result)
  }
  
  def measure(n: Long) = {
    val start = time
    loop (n) {
      fub.function(fub.sample)
    }
    val end = time
    timeDiff(start, end)
  }
}

class Counter2[A, B, C](fub: Fub2[A, B, C]) extends Counter {
  def exec(n: Long): Unit = loop(n) {
    val result = fub.function(fub.sample._1, fub.sample._2)
    accumulate(result)
  }
  
  def measure(n: Long) = {
    val start = time
    loop (n) {
      fub.function(fub.sample._1, fub.sample._2)
    }
    val end = time
    timeDiff(start, end)
  }
}

class Counter3[A, B, C, D](fub: Fub3[A, B, C, D]) extends Counter {
  def exec(n: Long): Unit = loop(n) {
    val result = fub.function(fub.sample._1, fub.sample._2, fub.sample._3)
    accumulate(result)
  }
  
  def measure(n: Long) = {
    val start = time
    loop (n) {
      fub.function(fub.sample._1, fub.sample._2, fub.sample._3)
    }
    val end = time
    timeDiff(start, end)
  }
}

