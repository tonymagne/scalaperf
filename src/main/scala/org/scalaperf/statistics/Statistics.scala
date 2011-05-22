package org.scalaperf
package statistics

import scala.Double.NaN

/**
 * @param sample     the data
 * @param mean       the mean (a.k.a the average)
 * @param meanLower  mean lower bound (confidence interval)
 * @param meanUpper  mean upper bound (confidence interval)
 * @param sd         the standard deviation
 * @param sdLower    sd lower bound (confidence interval)
 * @param sdUpper    sd upper bound (confidence interval)
 * @param percentile the p percentile
 */
case class Stats(first:      Double,
                 mean:       Double, 
                 meanLower:  Double, 
                 meanUpper:  Double, 
                 sd:         Double, 
                 sdLower:    Double, 
                 sdUpper:    Double,
                 percentile: Percentile) {
  require(mean != NaN,      "Mean is NaN")
  require(mean >= 0,        "Mean must be greater than or equals to 0")
  require(meanLower != NaN, "MeanLower is NaN")
  require(meanLower >= 0,   "Mean must be MeanLower than or equals to 0")
  require(meanUpper != NaN, "MeanUpper is NaN")
  require(meanUpper >= 0,   "MeanUpper must be greater than or equals to 0")
  require(sd != NaN,        "Standard Deviation is NaN")
  require(sd >= 0,          "Sandard Deviation must be greater than or equals to 0")
  require(sdLower != NaN,   "SdLower is NaN")
  require(sdLower >= 0,     "SdLower must be greater than or equals to 0")
  require(sdUpper != NaN,   "SdUpper is NaN")
  require(sdUpper >= 0,     "SdUpper must be greater than or equals to 0")
}

case class Estimate(name: Symbol, point: Double, lower: Double, upper: Double, confidenceLevel: Double) {
  require(lower != NaN,   "lower is NaN")
  require(upper != NaN,   "upper is NaN")
  require(lower <= upper, "lower = " + lower + " > upper = " + upper)
  require(confidenceLevel >= 0 && confidenceLevel <= 1, "Confidence Level = " + confidenceLevel + " is an illegal value")
}

case class Percentile(p: Int, value: Double)

object Statistics {
  import org.apache.commons.math.stat.descriptive.moment.{Mean, StandardDeviation}
  import org.apache.commons.math.stat.descriptive.rank.{Percentile => CommonsPercentile, Median}
  import scala.math.floor
  
  implicit def arrayOfLongToStatsArrayOfDouble(values: Array[Long])     = new StatsArrayOfDouble(values.map(_.toDouble))
  implicit def arrayOfDoubleToStatsArrayOfDouble(values: Array[Double]) = new StatsArrayOfDouble(values)
  
  final class StatsArrayOfDouble(adaptee: Array[Double]) {
    def mean = (new Mean).evaluate(adaptee)
    
    def sd = (new StandardDeviation).evaluate(adaptee)
    
    def median = (new Median).evaluate(adaptee)
        
    def percentile(p: Int) = Percentile(p, new CommonsPercentile(p).evaluate(adaptee))
    
    def quantile(k: Int, q: Int): Double = {
      require(q >= 2, "q = " + q + " < 2")
      require(k < q,  "k = " + k + " >= q = " + q)
      
      if (adaptee.length == 1) adaptee(0)
      else {
        val sorted = adaptee.sortWith(_ < _)
        val p = (k.toDouble / q)
        val index = (sorted.length - 1) * p
        val j = floor(index).toInt
        val g = index - j
        ( sorted(j) + (g * (sorted(j + 1) - sorted(j))) )
      }
    }
  }

}
