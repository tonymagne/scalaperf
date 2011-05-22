package org.scalaperf
package bench

import implicits.toValueGenerator
import statistics.Stats
import fub.Fub1
import scala.math.{sqrt, min}

object NoiseDetector {
  class Lfsr {
    var register: Int = 1
    val mask: Int = ~0 // thirty two 1's in binary
    val taps: Int = (1 << 31) | (1 << 30) | (1 << 29) | (1 << 9)
    
    def advance(nbTransitions: Long): Int = {
      var i = nbTransitions
      while (i > 0) {
        register = ((register >>> 1) ^ (-(register & 1) & taps)) & mask
        i = i - 1
      }
      register
    }
  }
  
  def detectEnvironmentalNoise(stats: Stats, myConfig: BenchConfig): Option[Double]  = {
    if (!myConfig.estimateNoiseFloor) None
    else {
      val lfsr = new Lfsr
      val nbTransitions = 1L * 1000L * 1000L
      val fub = new Fub1[Long, Int] {
        override def function = lfsr.advance
        override def generator = nbTransitions
      }
      val bench = new Stopwatch1[Long, Int](fub) with Benchmark[Int] with Configuration with BenchLogger {
        override def config = myConfig
      }
      bench.log("Detecting environmental noise...")
      val (_, firstMeasurement, measurements, _) = bench.doBenchmark()
      val noiseFirst = firstMeasurement.executionTime.toDouble
      val noiseSample = measurements.map(_.executionTime.toDouble).toArray
      val noiseStats = computeStats(noiseFirst, noiseSample, myConfig)
      val sd      = stats.sd / sqrt(stats.mean)
      val noiseSd = noiseStats.sd / sqrt (noiseStats.mean)
      val sdFraction = min(noiseSd / sd, 1)
      if (sdFraction > myConfig.sdFractionThreshold) Some(sdFraction)
      else None
    }
  }
}