package org.scalaperf
package bench

import scala.annotation.tailrec
import utils.Memory
import utils.{Jvm, JvmState}

trait BenchConfig {
  private[bench] final def nanosecondPerSecond = 1e9
  def warmupTime = 10
  private[bench] final def warmupTimeNs = warmupTime * nanosecondPerSecond
  def warmupIterationNb = 0
  private[bench] final def warmupIterationNbEnabled = warmupIterationNb > 0
  def nbMeasurements = 60
  def nbActions = 1
  def executionTimeGoal = 1
  private[bench] final def executionTimeGoalNs = executionTimeGoal * nanosecondPerSecond
  def confidenceLevel = 0.95
  def estimateNoiseFloor = false
  def sdFractionThreshold = 0.01
  def cleanFractionThreshold = 0.01
  def percentil = 99
  def logEnabled = false
}

trait Configuration {
  def config: BenchConfig
}

private[bench] trait BenchLogger { this: Configuration =>
  def log(message: String): Unit = {
    if (config.logEnabled) println("INFO: " + message)
  }
}

trait Benchmark[T] { this: Stopwatch[T] with Configuration with BenchLogger =>

  def doBenchmark(): (Int, Measurement, List[Measurement], Option[Double]) = {
    log("Performing initial measurement...")
    val firstMeasurement = once()
    val nbWarmupExec = if (config.warmupIterationNbEnabled) warmupIteration() else warmup()
    val nbExec = inferNbExecution()
    val measurements = doMeasure(nbExec)
    val cleanupStatus = cleanFinal(measurements)
    (nbExec, firstMeasurement, measurements, cleanupStatus)
  }
  
  def once(): Measurement = {
    Memory.restore
    measure(1)._2
  }
  
  private def doMeasure(nbExecution: Int): List[Measurement] = {
    log("Performing %d measurements (the function under benchmark being executed %s for each measurement)...".format(config.nbMeasurements, if (nbExecution <= 1) "once" else nbExecution + " times"))
	  @tailrec
    def doMeasureTr(index: Int, initialState: JvmState, measurements: List[Measurement]): List[Measurement] = index match {
      case 0 => measurements
      case i => {
        val measurement = measure(nbExecution)
        measurement match {
          case (_, Measurement(_), currentState) if (initialState != currentState) =>
            val stateDiff = Jvm.diff(initialState, currentState).getOrElse("")
            log("Doing measurement #%d/%d (restarting because the JVM state has changed: %s).".format(config.nbMeasurements - index + 1, config.nbMeasurements, stateDiff))
            doMeasureTr(config.nbMeasurements, currentState, List[Measurement]())
          case (_, Measurement(_), currentState)                                   =>
            log("Doing measurement #%d/%d.".format(config.nbMeasurements - index + 1, config.nbMeasurements))
            doMeasureTr(i - 1, initialState, measurement._2 :: measurements) 
        }
      }
    }
    
    Memory.restore
    doMeasureTr(config.nbMeasurements, Jvm.state, List[Measurement]())
  }
  
  private def warmup(): Long = {
    log("Performing %d seconds warmup...".format(config.warmupTime))
    val start = time
    @tailrec
    def warmupTr(n: Long, duration: Long, currentMesure: (List[T], Measurement, JvmState)): Long = {
      if (duration >= config.warmupTimeNs) n
      else warmupTr(n * 2, timeDiff(start, time), measure(n * 2))
    }    
    
    Memory.restore
    warmupTr(1, 0, measure(1))
  }
  
  private def warmupIteration(): Long = {
    log("Performing %d warmup iterations...".format(config.warmupIterationNb))
    Memory.restore
    for (i <- 0 until config.warmupIterationNb) measure(1)
    config.warmupIterationNb
  }
  
  private def inferNbExecution(): Int = {
    log("Determining how many executions are required...")
	  @tailrec
    def inferNbExecutionTr(n: Int, initialState: JvmState, currentMeasurement: (List[T], Measurement, JvmState)): Int = currentMeasurement match {
      case (_, _, currentState) if (initialState != currentState) =>
        val stateDiff = Jvm.diff(initialState, currentState).getOrElse("")
        log("Redoing %d because because the JVM state has changed: %s.".format(n, stateDiff))
        inferNbExecutionTr(n, currentState, measure(n))
      case (_, Measurement(executionTime), _) =>    
        if (executionTime > config.executionTimeGoalNs) n 
        else {
          log("Trying %d".format(n * 2))
          inferNbExecutionTr(n * 2, initialState, measure(n * 2))
        }
    }
    
    Memory.restore
    inferNbExecutionTr(1, Jvm.state, measure(1))
  }
  
  def cleanFinal(measurements: List[Measurement]): Option[Double] = {
    log("Performing a final JVM clean...")
    val start = time
    Memory.restore
    val end = time
    val timeClean = timeDiff(start, end)
    val timeRun = measurements.foldLeft(0L)((acc, m) => acc + m.executionTime)
    val cleanFraction = if (timeRun == 0) 1.0 else timeClean / timeRun
    if (cleanFraction > config.cleanFractionThreshold) Some(cleanFraction)
    else None
  }
}
