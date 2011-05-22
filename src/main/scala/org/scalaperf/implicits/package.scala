package org.scalaperf

import generator.{Generator, Gen}
import bench.BenchResult
import format.HumanFormatter

package object implicits {
  /**
   * Implicit conversion of a value to a ValueGen
   */
  implicit def toValueGenerator[A](a: A): Generator[A]  = Gen.value(a)
  
  /**
   * Implicit conversion of an Array to a RandomGen
   */
  implicit def toRandomGenerator[A](a: Array[A]): Generator[A] = Gen.random(a)
  
  /**
   * Implicit conversion of a function to a CustomGen
   */
  implicit def toCustomGenerator[A](next: () => A): Generator[A] = Gen.custom(next)
  
  
  /**
   * Implicit conversion of a BenchResult to a HumanFormat
   */
  implicit def toHumanFormat(res: BenchResult) = new HumanFormatter(res)
}