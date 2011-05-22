package org.scalaperf
package generator

import scala.util.Random

trait Generator[A] {
  def sample: A
}

private[generator] class ValueGen[A](value: A) extends Generator[A] {
  override def sample = value
}

private[generator] class RandomGen[A](values: Array[A]) extends Generator[A] {
  val random = new Random
  
  override def sample = values(random.nextInt(values.length))
}

private[generator] class CustomGen[A](next: () => A) extends Generator[A] {
  override def sample = next()
}

object Gen {
  def value[A](value: A) = new ValueGen(value)
  def random[A](values: Array[A]) = new RandomGen(values)
  def custom[A](next: () => A) = new CustomGen(next)
}
