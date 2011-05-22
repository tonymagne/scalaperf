package org.scalaperf
package statistics

import org.apache.commons.math.random.MersenneTwister
import org.apache.commons.math.distribution.NormalDistributionImpl
import Statistics.arrayOfDoubleToStatsArrayOfDouble
import scala.math.{min, max, round, pow}

/**
 * Scala translation of Brent Boyer code (bb.science.Bootstrap)
 * http://www.ellipticgroup.com/html/benchmarkingArticle.html
 */
class Bootstrap(sample: Array[Double], nbResamples: Int = 100 * 1000, confidenceLevel: Double = 0.95) {
  require(nbResamples >= 0, "Nb Resample must be greater or equal to 0")
  require(confidenceLevel >= 0 && confidenceLevel <= 1, "Confidence Level = " + confidenceLevel + " is an illegal value")

  def apply(): Map[Symbol, Estimate] = apply(Map('Mean   -> (_.mean), 
                                                 'Median -> (_.median), 
                                                 'Sd     -> (_.sd)))
  
  def apply(estimators: Map[Symbol, Array[Double] => Double]): Map[Symbol, Estimate] = {
    val resampleMap = doResampling(estimators)
    val normalDistribution = new NormalDistributionImpl
    val alpha = 1 - confidenceLevel
    val z1 = normalDistribution.inverseCumulativeProbability(alpha / 2)
    val z2 = -z1
    
    estimators.map { case (key, estimator) =>
      val point = estimator(sample)
      if (sample.length == 1) (key, Estimate(key, point, point, point, confidenceLevel))
      else {
        val resampleEstimates = resampleMap(key)
        
        // @see bb.science.Bootstrap.calcBias(double, double[], NormalDistribution)
        def calculateBias() = {
          val probability = resampleEstimates.count(_ > point).toDouble / nbResamples
          normalDistribution.inverseCumulativeProbability(probability)
        }
        
        // @see bb.science.Bootstrap.calcAcceleration(Estimator)
        def calculateAcceleration() = {
          val jackknifeEstimates = calculateJackknifeEstimates()
          val jackknifeMean = jackknifeEstimates.mean
          val (sumOfSquares, sumOfCubes) = jackknifeEstimates.foldLeft[(Double, Double)](0, 0) { (acc, d) =>
            val diff = jackknifeMean - d
            val diffSquared = diff * diff
            val diffCubed   = diff * diffSquared
            
            (acc._1 + diffSquared, acc._2 + diffCubed)
          }
          sumOfCubes / (6 * pow(sumOfSquares, 1.5))
        }
        
        // @see bb.science.Bootstrap.calcJackknifeEsts(Estimator)
        def calculateJackknifeEstimates() = {
          val sampleWithIndex = sample.zipWithIndex
          (for (i <- 0 until sample.length) yield sampleWithIndex.flatMap { case (d, index) =>
            if (i == index) Array[Double]() else Array(d)
          }).map(estimator(_)).toArray
        }
        
        val b = calculateBias()
        val a = calculateAcceleration()
        val bZ1 = b + z1
        val a1 = normalDistribution.cumulativeProbability( b + (bZ1 / (1 - (a * bZ1))) )
        val bZ2 = b + z2
        val a2 = normalDistribution.cumulativeProbability( b + (bZ2 / (1 - (a * bZ2))) )
        
        val indexLower = max(round(a1 * nbResamples), 0).toInt
        val indexUpper = min(round(a2 * nbResamples), nbResamples - 1).toInt
        
        (key, Estimate(key, point, resampleEstimates(indexLower), resampleEstimates(indexUpper), confidenceLevel))
      }
    }
  }
  
  // @see bb.science.Bootstrap.doResampling(Estimator[])
  private def doResampling(estimators: Map[Symbol, Array[Double] => Double]) = {
    import Utils.makeSeed
    
    val resample = new Array[Double](sample.length)
    val random = new MersenneTwister(makeSeed())
    
    val resamples = (for (key <- estimators.keys) yield (key, new Array[Double](nbResamples))).toMap 
    
    for (i <- 0 until nbResamples) {
      
      for (j <- 0 until sample.length) {
        resample(j) = sample(random.nextInt(sample.length))
      }
      
      estimators.foreach { case (key, estimator) =>
        resamples(key)(i) = estimator(resample)
      }
    }
    
    val res = resamples.map {
      case (key, resample) => (key, resample.sortWith( _< _))
    }
    
    res
  }
  
}