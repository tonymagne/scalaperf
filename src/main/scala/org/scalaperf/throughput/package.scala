package org.scalaperf

import fub.{Fub1, Fub2, Fub3}
import generator.Generator

package object throughput {
  private[throughput] val defaultConfig = new ThroughputConfig {}
  
  def throughput1[A, B](underBenchmark: A => B, gen: Generator[A], nbExecution: Long = 1) 
                      (implicit myConfig: ThroughputConfig = defaultConfig): Double = {
    val fub = new Fub1[A, B] {
      override def function = underBenchmark
      override def generator = gen
    }
    val throughputCounter = new Counter1[A, B](fub) with Throughput with Configuration {
      override def config = myConfig
    }
    throughputCounter.doCount(nbExecution)
  }
  
  def throughput2[A, B, C](underBenchmark: (A, B) => C, gen: Generator[(A, B)], nbExecution: Long = 1)
                          (implicit myConfig: ThroughputConfig = defaultConfig): Double = {
    val fub = new Fub2[A, B, C] {
      override def function = underBenchmark
      override def generator = gen
    }
    val throughputCounter = new Counter2[A, B, C](fub) with Throughput with Configuration {
      override def config = myConfig
    }
    throughputCounter.doCount(nbExecution)
  }
  
  def throughput3[A, B, C, D](underBenchmark: (A, B, C) => D, gen: Generator[(A, B, C)], nbExecution: Long = 1)
                       (implicit myConfig: ThroughputConfig  = defaultConfig): Double = {
    val fub = new Fub3[A, B, C, D] {
      override def function = underBenchmark
      override def generator = gen
    }
    val throughputCounter = new Counter3[A, B, C, D](fub) with Throughput with Configuration {
      override def config = myConfig
    }
    throughputCounter.doCount(nbExecution)
  }
}