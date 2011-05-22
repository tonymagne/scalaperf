package org.scalaperf
package throughput

import fub.{Fub1, Fub2, Fub3}
import generator.{Generator, Gen}

trait ThroughputConfig {
  final def nanosecondPerSecond = 1e9
  def nbExecution = 100
  def targetExecutionTimeEnabled = true
  def targetExecutionTime = 60.0
  final def targetExecutionTimeNs = targetExecutionTime * nanosecondPerSecond
}

trait Configuration {
  def config: ThroughputConfig
}

trait Throughput extends Configuration { this: Counter =>
  def doCount(nbExecution: Long): Double = {
    val nbExec = if (config.targetExecutionTimeEnabled) inferNbExecution else nbExecution
    counter.start
    exec(nbExec)
    counter ! Stop
    Thread.sleep(10)
    counter.get
  }
  
  private def inferNbExecution: Long = {
    def inferNbExecutionTr(n: Long, currentExecutionTime: Double): Long =  {
      if (currentExecutionTime >= config.targetExecutionTimeNs) n else inferNbExecutionTr(n * 2, measure(n * 2))
    }
    inferNbExecutionTr(1, measure(1))
  } 
}

