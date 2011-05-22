package org.scalaperf
package statistics

import org.specs2.mutable._
import scala.util.Random

object DiagnosticSpecs extends Specification {
  "The diagnostic object" should {
    "identify ouliers in the given sample." in {
      val sample = (for (i <- 0 until 60) yield {
        i match {
          case 0 => 0d
          case 1 => 1.25
          case 2 => 3.75
          case 3 => 5d
          case _ => 2 + Random.nextDouble
        }
      }).toArray
      
      val outliers = Diagnostics.diagnoseOutliers(sample)
      
      outliers.lowExtreme must contain((0d, 1))
      outliers.lowMild must contain((1.25, 2))
      
      outliers.highMild must contain((3.75, 59))
      outliers.highExtreme must contain((5, 60))
    }
    
    "identify serial correlation in the given sample" in {
      val sample = (for (i <- 0 until 60) yield ((1d / 60 * i) + Random.nextDouble)).toArray
      
      val serialCorrelation = Diagnostics.diagnoseSerialCorrelation(sample)
      serialCorrelation.size must beGreaterThan(0)
    }
    
    
  }
}