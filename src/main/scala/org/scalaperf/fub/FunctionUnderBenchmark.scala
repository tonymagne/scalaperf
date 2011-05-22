package org.scalaperf
package fub

import generator.Generator

/**
 * Function Under Benchmark: A => B
 */
trait Fub1[A, B] {
  def function: Function1[A, B]
  def sample: A = generator.sample
  def generator: Generator[A]
}

/**
 * Function Under Benchmark: (A, B) => C
 */
trait Fub2[A, B, C] {
  def function: Function2[A, B, C]
  def sample: Tuple2[A, B] = generator.sample
  def generator: Generator[Tuple2[A, B]]
}

/**
 * Function Under Benchmark: (A, B, C) => D
 */
trait Fub3[A, B, C, D] {
  def function: Function3[A, B, C, D]
  def sample: Tuple3[A, B, C] = generator.sample
  def generator: Generator[Tuple3[A, B, C]]
}

/**
 * Function Under Benchmark: (A, B, C, D) => E
 */
trait Fub4[A, B, C, D, E] {
  def function: Function4[A, B, C, D, E]
  def sample: Tuple4[A, B, C, D] = generator.sample
  def generator: Generator[Tuple4[A, B, C, D]]
}

/**
 * Function Under Benchmark: (A, B, C, D, E) => F
 */
trait Fub5[A, B, C, D, E, F] {
  def function: Function5[A, B, C, D, E, F]
  def sample: Tuple5[A, B, C, D, E] = generator.sample
  def generator: Generator[Tuple5[A, B, C, D, E]]
}