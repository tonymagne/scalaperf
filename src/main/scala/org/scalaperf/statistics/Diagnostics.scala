package org.scalaperf
package statistics

import scala.annotation.tailrec
import Statistics.arrayOfDoubleToStatsArrayOfDouble
import scala.math.{sqrt, round, min}

case class Outliers(median:      Double,
                    iqr:         Double,
                    lowExtreme:  List[(Double, Int)],
                    lowMild:     List[(Double, Int)],
                    highExtreme: List[(Double, Int)],
                    highMild:    List[(Double, Int)]) {
  
  def isEmpty: Boolean = (lowExtreme.isEmpty && lowMild.isEmpty && highExtreme.isEmpty && highMild.isEmpty)
}
                    
case class SerialCorrelation(N:                 Int,
                             nbOutsideExpected: Int,
                             K:                 Int,
                             outsideBelow:      List[(Double, Double, Int)],
                             outsideAbove:      List[(Double, Double, Int)]) {
  
  def isEmpty: Boolean = (outsideBelow.isEmpty && outsideAbove.isEmpty)
  
  def isMeaningless = (N < 50)
  
  def hasMoreThanExpected = (outsideBelow.size + outsideAbove.size) > nbOutsideExpected
  
  def size = (outsideBelow.size + outsideAbove.size)
}

/**
 * Scala translation of Brent Boyer code
 * http://www.ellipticgroup.com/html/benchmarkingArticle.html
 */
object Diagnostics {
  // @see bb.util.Stats.diagnoseSdOfActions(double)
  def diagnoseSdForAction(blockStats: Stats, nbAction: Int): Option[Double] = {
    import scala.math.{sqrt, min}
    
    if (nbAction < 16) return None
    
    val muB    = blockStats.mean
    val sigmaB = blockStats.sd
    
    val muA    = muB / nbAction
    val sigmaA = sigmaB / sqrt(nbAction)
    
    val tMin = 0
    val muGMin = (muA + tMin) / 2
    val sigmaG = min( (muGMin - tMin) / 4, sigmaA)
    
    if (sigmaB == 0) return None
    
    val cMax1 = cMaxSolver(nbAction, muB, sigmaB, muA, sigmaG, tMin)
    val cMax2 = cMaxSolver(nbAction, muB, sigmaB, muA, sigmaG, muGMin)
    val cMax = min(cMax1, cMax2)
    
    if (cMax == 0) return None
    
    val var1 = varianceOutliers(nbAction, sigmaB, sigmaG, 1)
    val var2 = varianceOutliers(nbAction, sigmaB, sigmaG, cMax)
    
    val cOutMin   = if (var1 < var2) 1L   else cMax
    val varOutMin = if (var1 < var2) var1 else var2
    
    val varBGOutMin = (sigmaB * sigmaB) - ((nbAction - cOutMin) * (sigmaG * sigmaG))
    val muGOutMin = muA - sqrt( (cOutMin * varBGOutMin) / (nbAction * (nbAction - cOutMin)))
    
    val UOutMin = muA + sqrt( ((nbAction - cOutMin) * varBGOutMin) / (nbAction * cOutMin))
    
    val fractionVarOutlierMin = varOutMin / (sigmaB * sigmaB)
    
    if (fractionVarOutlierMin < 0.01) None
    else                              Some(fractionVarOutlierMin)
  }
  
  // @see bb.util.Stats.cMaxSolver(double, double, double, double, double, double)
  private def cMaxSolver(a: Double, muB: Double, sigmaB: Double, muA: Double, sigmaG: Double, x: Double) = {
    import scala.math.{floor, sqrt}
    
    val k0 = -a * a * (muA - x) * (muA - x)
    val k1 = (sigmaB * sigmaB) - (a * sigmaG * sigmaG) + (a * (muA - x) * (muA -x))
    val k2 = sigmaG * sigmaG
    
    val determinant = (k1 * k1) - (4 * k2 * k0)
    val cMax = floor(-2 * k0 / (k1 + sqrt(determinant))).toLong
    
    cMax
  }
  
  // @see bb.util.Stats.varianceOutliers(double, double, double, double)
  private def varianceOutliers(a: Double, sigmaB: Double, sigmaG: Double, c: Double) = {
    ((a - c) / a) * ((sigmaB * sigmaB) - ((a - c) * (sigmaG * sigmaG)))
  }

  // @see bb.util.Benchmark.diagnoseOutliers()
  def diagnoseOutliers(sample: Array[Double]): Outliers = {
    val sorted    = sample.sortWith(_ < _)
    val quartile1 = sorted.quantile(1, 4)
    val median    = sorted.median
    val quartile3 = sorted.quantile(3, 4)
    val iqr       = quartile3 - quartile1
    
    val lowExtreme  = quartile1 - (3 * iqr)
    val lowMild     = quartile1 - (1.5 * iqr)
    val highExtreme = quartile3 + (3 * iqr)
    val highMild    = quartile3 + (1.5 * iqr)
    
    @tailrec
    def partition(data: List[(Double, Int)], current: Outliers): Outliers = {
      (data, current) match {
        case (Nil,          _)                                                            => current
        case ((d, i)::tail, Outliers(median, iqr, les, lms, hes, hms)) if d < lowExtreme  => partition(tail, Outliers(median, iqr, (d, i + 1)::les, lms, hes, hms))
        case ((d, i)::tail, Outliers(median, iqr, les, lms, hes, hms)) if d < lowMild     => partition(tail, Outliers(median, iqr, les, (d, i + 1)::lms, hes, hms))
        case ((d, i)::tail, Outliers(median, iqr, les, lms, hes, hms)) if d > highExtreme => partition(tail, Outliers(median, iqr, les, lms, (d, i + 1)::hes, hms))
        case ((d, i)::tail, Outliers(median, iqr, les, lms, hes, hms)) if d > highMild    => partition(tail, Outliers(median, iqr, les, lms, hes, (d, i + 1)::hms))
        case (_::tail, outliners)                                                         => partition(tail, outliners)
      }
    }
    
    val ols = partition(sorted.toList.zipWithIndex, Outliers(median, iqr, Nil, Nil, Nil, Nil))
    Outliers(median,
             iqr,
             ols.lowExtreme.reverse,
             ols.lowMild.reverse,
             ols.highExtreme.reverse,
             ols.highMild.reverse)
  }
  
  // @see bb.util.Benchmark.diagnoseSerialCorrelation()
  def diagnoseSerialCorrelation(sample: Array[Double]): SerialCorrelation = {
    val N = sample.length

    if (N < 50) SerialCorrelation(N, 0, 0, Nil, Nil)
    else {
      val (r, ciLower, ciUpper) = autocorrelation(sample)
      val K = min(round(N / 4d).toInt, 20)
      val nbOutsideExpected = round( (1 - 0.95) * K ).toInt
      
      @tailrec
      def partition(data: List[(Double, Int)], current: SerialCorrelation): SerialCorrelation = {
        (data, current) match {
          case (Nil, _)                                                                                                 =>
            current
          case ((_, k)::tail, SerialCorrelation(N, nbExpected, K, below, above)) if k == 0                                 => 
            partition(tail, SerialCorrelation(N, nbExpected, K, below, above))
          case ((d, k)::tail, SerialCorrelation(N, nbExpected, K, below, above)) if r(k) < ciLower(k) || r(k) > ciUpper(k) =>
            val mean  = (ciUpper(k) + ciLower(k)) / 2
            val sigma = (ciUpper(k) - ciLower(k)) / (2 * 1.96)
            val diff  = r(k) - mean
            val scale = diff / sigma
            if (diff > 0) partition(tail, SerialCorrelation(N, nbExpected, K, below, (r(k), scale, k)::above))
            else          partition(tail, SerialCorrelation(N, nbExpected, K, (r(k), scale, k)::below, above))
          case (_::tail, SerialCorrelation(N, nbExpected, K, below, above))                                               =>
            partition(tail, SerialCorrelation(N, nbExpected, K, below, above))
        }
      }
      val sc = partition(r.toList.zipWithIndex, SerialCorrelation(N, nbOutsideExpected, K, Nil, Nil))
      SerialCorrelation(sc.N,
                        sc.nbOutsideExpected, 
                        sc.K, 
                        sc.outsideBelow.reverse, 
                        sc.outsideAbove.reverse)
    }
  }
  
  // @see bb.science.Math2.autocorrelation(double[])
  private def autocorrelation(numbers: Array[Double]): (Array[Double], Array[Double], Array[Double]) = {
    val N = numbers.length
    val c = autocovariance(numbers)
    
    val r = c.zipWithIndex.map { case (value, index) =>
      if (index == 0) 1d
      else            value / c(0)
    }
    
    val llse = r.zipWithIndex.map { case (_, index) =>
      if (index == 0) (0d, 0)
      else {
        val vark = Some((1 until index).foldLeft(0d) { (vark, i) =>
          vark + (r(i) * r(i))
        }).map(_ * 2).map(_ + 1).map(_ / N).map(sqrt(_)).get
        (vark, index)
      }
    }
    
    val meanr = -1d / N
    
    def ci(op: (Double, Double) => Double) = {
      llse.map { case (value, index) =>
        if (index == 0)  1d
        else {
          val delta = 1.96 * value
          op(meanr, delta)
        }
      }
    }
    
    val ciLower = ci(_ -_)
    val ciUpper = ci(_ + _)
    
    (r, ciLower, ciUpper)
  }
  
  // @see bb.science.Math2.autocovariance(double[])
  private def autocovariance(numbers: Array[Double]): Array[Double] = {
    val mean = numbers.mean
    val N = numbers.length
    
    (0 until N - 1).map { k =>
      val sum = (0 until N - k).foldLeft[Double](0) { (sum, i) =>
        sum + ((numbers(i) - mean) * (numbers(i + k) - mean)) 
      }
      sum / N
    }.toArray
  }

}