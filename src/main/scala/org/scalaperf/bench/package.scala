package org.scalaperf

import statistics.{Stats, Bootstrap, Diagnostics}
import statistics.Statistics.arrayOfDoubleToStatsArrayOfDouble
import fub.{Fub1, Fub2, Fub3, Fub4, Fub5}
import generator.Generator

package object bench {
  private[bench] val defaultConfig = new BenchConfig {}
  
  
  /**
   * Measures the execution time of the provided function.
   * Only one measurement is performed.
   * 
   * @param underBenchmark the function
   * @param gen            the parameter generator
   * @returns the execution time
   */
  def measureOnce1[A, B](underBenchmark: A => B, gen: Generator[A])
                       (implicit myConfig: BenchConfig = defaultConfig): Long = {
    val fub = new Fub1[A, B] {
      override def function = underBenchmark
      override def generator = gen
    }
    val bench = new Stopwatch1[A, B](fub) with Benchmark[B] with Configuration with BenchLogger {
      override def config = myConfig
    }
    bench.once.executionTime
  }
  
  /**
   * Measures the execution time of the provided function.
   * Only one measurement is performed.
   * 
   * @param underBenchmark the function
   * @param gen            the parameter generator
   * @returns the execution time
   */
  def measureOnce2[A, B, C](underBenchmark: (A, B) => C, gen: Generator[(A, B)])
                           (implicit myConfig: BenchConfig = defaultConfig): Long = {
    val fub = new Fub2[A, B, C] {
      override def function = underBenchmark
      override def generator = gen
    }
    val bench = new Stopwatch2[A, B, C](fub) with Benchmark[C] with Configuration with BenchLogger {
      override def config = myConfig
    }
    bench.once.executionTime
  }
  
  /**
   * Measures the execution time of the provided function.
   * Only one measurement is performed.
   * 
   * @param underBenchmark the function
   * @param gen            the parameter generator
   * @returns the execution time
   */
  def measureOnce3[A, B, C, D](underBenchmark: (A, B, C) => D, gen: Generator[(A, B, C)])
                           (implicit myConfig: BenchConfig = defaultConfig): Long = {
    val fub = new Fub3[A, B, C, D] {
      override def function = underBenchmark
      override def generator = gen
    }
    val bench = new Stopwatch3[A, B, C, D](fub) with Benchmark[D] with Configuration with BenchLogger {
      override def config = myConfig
    }
    bench.once.executionTime
  }
  
  /**
   * Measures the execution time of the provided function.
   * Only one measurement is performed.
   * 
   * @param underBenchmark the function
   * @param gen            the parameter generator
   * @returns the execution time
   */
  def measureOnce4[A, B, C, D, E](underBenchmark: (A, B, C, D) => E, gen: Generator[(A, B, C, D)])
                           (implicit myConfig: BenchConfig = defaultConfig): Long = {
    val fub = new Fub4[A, B, C, D, E] {
      override def function = underBenchmark
      override def generator = gen
    }
    val bench = new Stopwatch4[A, B, C, D, E](fub) with Benchmark[E] with Configuration with BenchLogger {
      override def config = myConfig
    }
    bench.once.executionTime
  }

  /**
   * Measures the execution time of the provided function.
   * Only one measurement is performed.
   * 
   * @param underBenchmark the function
   * @param gen            the parameter generator
   * @returns the execution time
   */
  def measureOnce5[A, B, C, D, E, F](underBenchmark: (A, B, C, D, E) => F, gen: Generator[(A, B, C, D, E)])
                           (implicit myConfig: BenchConfig = defaultConfig): Long = {
    val fub = new Fub5[A, B, C, D, E, F] {
      override def function = underBenchmark
      override def generator = gen
    }
    val bench = new Stopwatch5[A, B, C, D, E, F](fub) with Benchmark[F] with Configuration with BenchLogger {
      override def config = myConfig
    }
    bench.once.executionTime
  }
  
  /**
   * Benchmarks the provided function.
   * 
   * @param underBenchmark the function
   * @param gen            the parameter generator
   * @returns the execution times statistics
   */
  def benchmark1[A, B](underBenchmark: A => B, gen: Generator[A])
                      (implicit myConfig: BenchConfig = defaultConfig): BenchResult = {
    val fub = new Fub1[A, B] {
      override def function = underBenchmark
      override def generator = gen
    }
    val bench = new Stopwatch1[A, B](fub) with Benchmark[B] with Configuration with BenchLogger {
      override def config = myConfig
    }
    val measures = bench.doBenchmark()
    diagnose(measures, myConfig)
  }
  
  /**
   * Benchmarks the provided function.
   * 
   * @param underBenchmark the function
   * @param gen            the parameter generator
   * @returns the execution times statistics
   */
  def benchmark2[A, B, C](underBenchmark: (A, B) => C, gen: Generator[(A, B)])
                         (implicit myConfig: BenchConfig = defaultConfig): BenchResult = {
    val fub = new Fub2[A, B, C] {
      override def function = underBenchmark
      override def generator = gen
    }
    val bench = new Stopwatch2[A, B, C](fub) with Benchmark[C] with Configuration with BenchLogger {
      override def config = myConfig
    }
    val measures = bench.doBenchmark()
    diagnose(measures, myConfig)
  }

  /**
   * Benchmarks the provided function.
   * 
   * @param underBenchmark the function
   * @param gen            the parameter generator
   * @returns the execution times statistics
   */
  def benchmark3[A, B, C, D](underBenchmark: (A, B, C) => D, gen: Generator[(A, B, C)])
                            (implicit myConfig: BenchConfig = defaultConfig): BenchResult = {
    val fub = new Fub3[A, B, C, D] {
      override def function = underBenchmark
      override def generator = gen
    }
    val bench = new Stopwatch3[A, B, C, D](fub) with Benchmark[D] with Configuration with BenchLogger {
      override def config = myConfig
    }
    val measures = bench.doBenchmark()
    diagnose(measures, myConfig)
  }
  
  /**
   * Benchmarks the provided function.
   * 
   * @param underBenchmark the function
   * @param gen            the parameter generator
   * @returns the execution times statistics
   */
  def benchmark4[A, B, C, D, E](underBenchmark: (A, B, C, D) => E, gen: Generator[(A, B, C, D)])
                            (implicit myConfig: BenchConfig = defaultConfig): BenchResult = {
    val fub = new Fub4[A, B, C, D, E] {
      override def function = underBenchmark
      override def generator = gen
    }
    val bench = new Stopwatch4[A, B, C, D, E](fub) with Benchmark[E] with Configuration with BenchLogger {
      override def config = myConfig
    }
    val measures = bench.doBenchmark()
    diagnose(measures, myConfig)
  }
  
   /**
   * Benchmarks the provided function.
   * 
   * @param underBenchmark the function
   * @param gen            the parameter generator
   * @returns the execution times statistics
   */
  def benchmark5[A, B, C, D, E, F](underBenchmark: (A, B, C, D, E) => F, gen: Generator[(A, B, C, D, E)])
                            (implicit myConfig: BenchConfig = defaultConfig): BenchResult = {
    val fub = new Fub5[A, B, C, D, E, F] {
      override def function = underBenchmark
      override def generator = gen
    }
    val bench = new Stopwatch5[A, B, C, D, E, F](fub) with Benchmark[F] with Configuration with BenchLogger {
      override def config = myConfig
    }
    val measures = bench.doBenchmark()
    diagnose(measures, myConfig)
  }
  
  private[bench] def computeStats(first: Double, measurements: Array[Double], myConfig: BenchConfig): Stats = {
    val estimations = new Bootstrap(sample          = measurements, 
                                    confidenceLevel = myConfig.confidenceLevel)()
    Stats(first,
          estimations('Mean).point, 
          estimations('Mean).lower, 
          estimations('Mean).upper, 
          estimations('Sd).point,
          estimations('Sd).lower,
          estimations('Sd).upper,
          measurements.percentile(myConfig.percentil))
  }
  
  private[bench] def forActions(blockStats: Stats, nbExecution: Long): Stats = {
    import scala.math.sqrt
    import statistics.Percentile
    val meanFactor = 1.0 / nbExecution
    val sdFactor = 1.0 / sqrt(nbExecution) 
    Stats(blockStats.first,
          blockStats.mean      * meanFactor,
          blockStats.meanLower * meanFactor,
          blockStats.meanUpper * meanFactor,
          blockStats.sd      * sdFactor,
          blockStats.sdLower * sdFactor,
          blockStats.sdUpper * sdFactor,
          Percentile(blockStats.percentile.p, blockStats.percentile.value * meanFactor))
  }
  
  private[bench] def diagnose(rawData: (Int, Measurement, List[Measurement], Option[Double]), myConfig: BenchConfig): BenchResult = {
    import statistics.Diagnostics._
    import NoiseDetector._
    
    val (nbExecution, first, measurements, lastCleanupStatus) = rawData
    val firstExecutionTime    = first.executionTime.toDouble
    val sample                = measurements.map(_.executionTime.toDouble).toArray
    val blockStats            = computeStats(firstExecutionTime, sample, myConfig)
    val actionStats           = forActions(blockStats, nbExecution * myConfig.nbActions)
    val fractionVarOutlierMin = diagnoseSdForAction(blockStats, nbExecution * myConfig.nbActions)
    val outliers              = diagnoseOutliers(sample)
    val serialCorrelation     = diagnoseSerialCorrelation(sample)
    val environmentalNoise    = detectEnvironmentalNoise(blockStats, myConfig)
    
    BenchResult(sample,
                blockStats,
                actionStats,
                lastCleanupStatus,
                fractionVarOutlierMin,
                outliers,
                serialCorrelation,
                environmentalNoise)
  }
  
  // TODO move to an appropriate location
  def scalaperfInfo(benchmarkName: String = ""): String = {
    import System.{getProperty, getenv}
    Array("Scala Perf: %s".format(benchmarkName),
          "%s (version %s)".format(getProperty("os.name"), getProperty("os.version")),
          if (getenv("PROCESSOR_IDENTIFIER") != null) "%s (%s cores)".format(getenv("PROCESSOR_IDENTIFIER"), getenv("NUMBER_OF_PROCESSORS")) else "",
          "%s (build %s)".format(getProperty("java.runtime.name"), getProperty("java.runtime.version")),
          "%s (build %s, %s)".format(getProperty("java.vm.name"), getProperty("java.vm.version"), getProperty("java.vm.info"))).filter(_ != "").mkString("\n")
  }
}