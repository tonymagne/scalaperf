package org.scalaperf
package bench

import utils.{Jvm, JvmState}
import time.Time
import fub.{Fub1, Fub2, Fub3, Fub4, Fub5}

trait Stopwatch[T] extends Time {
  def measure(n: Long): (List[T], Measurement, JvmState)
}

/**
 * TODO Re-factor: Try to get rid of the duplications
 * Warning: we do not want to introduce performance overhead.
 * We want to measure the function, not a call that call the function.
 * The while loop is a low level construct so the overhead here should be minimal
 */
class Stopwatch1[A, B](fub: Fub1[A, B]) extends Stopwatch[B] {
  override def measure(n: Long): (List[B], Measurement, JvmState) = {
    var i = n
    val sample = fub.sample
    val start = time()
    while (i > 0) {
      val out: B = fub.function(sample)
      i = i - 1
    }
    val end = time()
    
    (Nil, Measurement(timeDiff(start, end)), Jvm.state)
  }
}

class Stopwatch2[A, B, C](fub: Fub2[A, B, C]) extends Stopwatch[C] {
  override def measure(n: Long): (List[C], Measurement, JvmState) = {
    var i = n
    val sample = fub.sample
    val start = time()
    while (i > 0) {
      val out: C = fub.function(sample._1, sample._2)
      i = i - 1
    }
    val end = time()
    (Nil, Measurement(timeDiff(start, end)), Jvm.state)
  }
}

class Stopwatch3[A, B, C, D](fub: Fub3[A, B, C, D]) extends Stopwatch[D] {
  override def measure(n: Long): (List[D], Measurement, JvmState) = {
    var i = n
    val sample = fub.sample
    val start = time()
    while (i > 0) {
      val out = fub.function(sample._1, sample._2, sample._3)
      i = i - 1
    }
    val end = time()
    (Nil, Measurement(timeDiff(start, end)), Jvm.state)
  }
}

class Stopwatch4[A, B, C, D, E](fub: Fub4[A, B, C, D, E]) extends Stopwatch[E] {
  override def measure(n: Long): (List[E], Measurement, JvmState) = {
    var i = n
    val sample = fub.sample
    val start = time()
    while (i > 0) {
      val out = fub.function(sample._1, sample._2, sample._3, sample._4)
      i = i - 1
    }
    val end = time()
    (Nil, Measurement(timeDiff(start, end)), Jvm.state)
  }
}

class Stopwatch5[A, B, C, D, E, F](fub: Fub5[A, B, C, D, E, F]) extends Stopwatch[F] {
  override def measure(n: Long): (List[F], Measurement, JvmState) = {
    var i = n
    val sample = fub.sample
    val start = time()
    while (i > 0) {
      val out = fub.function(sample._1, sample._2, sample._3, sample._4, sample._5)
      i = i - 1
    }
    val end = time()
    (Nil, Measurement(timeDiff(start, end)), Jvm.state)
  }
}

case class Measurement(executionTime: Long) {
  require(executionTime >= 0, "The execution time must be greater or equal to 0")
}
  
