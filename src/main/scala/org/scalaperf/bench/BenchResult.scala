package org.scalaperf
package bench

import statistics.{Stats, Outliers, SerialCorrelation}

/**
 * @param sample                measurements
 * @param blockStats            block statistics
 * @param actionStats           action statistics
 * @param cleanFration          measurements time vs clean time (measurements time validity)
 * @param fractionVarOutlierMin action statistic sd validity (variance percentage)
 * @param outliers              outliers
 * @param serialCorrelation     serial correlation
 * @param environmentalNoise    environmental noise (sd validity)
 */

case class BenchResult(sample:                Array[Double],
                       blockStats:            Stats,
                       actionStats:           Stats,
                       cleanFraction:         Option[Double],
                       fractionVarOutlierMin: Option[Double],
                       outliers:              Outliers,
                       serialCorrelation:     SerialCorrelation,
                       environmentalNoise:    Option[Double])