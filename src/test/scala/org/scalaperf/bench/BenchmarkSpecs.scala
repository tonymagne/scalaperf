package org.scalaperf
package bench

import org.specs2.mutable._

object BenchmarkSpecs extends Specification {
  "The benchmark" should {

    "generate statistics for fonctions of one parameter" in {
      import implicits.toValueGenerator
      
      val f: Int => Int = (x: Int) => {
        Thread.sleep(10)
        x + 1
      }
      
      val res = benchmark1(f, 3)
      val tenMs: Double = 1e7
      res.actionStats.mean must beCloseTo(tenMs, 3 * res.actionStats.sd)
    }
    
    "generate statistics for fonctions of one parameter randomly chosen" in {
      import implicits.toRandomGenerator
      
      val f: Int => Int = (x: Int) => {
        Thread.sleep(10)
        x + 1
      }
      
      val res = benchmark1(f, Array(3, 6, 7, 9))
      val tenMs: Double = 1e7
      res.actionStats.mean must beCloseTo(tenMs, 3 * res.actionStats.sd)
    }

    "generate statistics for fonctions of one parameter using a custom generator" in {
      import implicits.toCustomGenerator
      
      val f: Int => Int = (x: Int) => {
        Thread.sleep(10)
        x + 1
      }
      
      var i = 0
      val gen = () => {i = i + 1; i}
      
      val res = benchmark1(f, gen)
      val tenMs: Double = 1e7
      res.actionStats.mean must beCloseTo(tenMs, 3 * res.actionStats.sd)
    }
    
    "generate statistics for fonctions of two parameters" in {
      import implicits.toValueGenerator
      
      val f = (x: Int, y: Int) => {
        Thread.sleep(10)
        x + y
      }
      
      val res = benchmark2(f, (18, 12))
      val tenMs: Double = 1e7
      res.actionStats.mean must beCloseTo(tenMs, 3 * res.actionStats.sd)
    }
    
    "generate statistics for fonctions of two parameters randomly chosen" in {
      import implicits.toRandomGenerator
      
      val f = (x: Int, y: Int) => {
        Thread.sleep(10)
        x + y
      }
      
      val res = benchmark2(f, Array((18, 12), (23, 6), (5, 9), (43, 2), (64, 55)))
      val tenMs: Double = 1e7
      res.actionStats.mean must beCloseTo(tenMs, 3 * res.actionStats.sd)
    }

    "generate statistics for fonctions of two parameters using a custom generator" in {
      import implicits.toCustomGenerator
      
      val f = (x: Int, y: Int) => {
        Thread.sleep(10)
        x + y
      }
      
      var i = 0
      def j = (i * 2) % 3
      val gen = () => { i = i + 1; (i, j)}
      
      val res = benchmark2(f, gen)
      val tenMs: Double = 1e7
      res.actionStats.mean must beCloseTo(tenMs, 3 * res.actionStats.sd)
    }
    
    "generate statistics for fonctions of three parameters" in {
      import implicits.toValueGenerator
      
      val f = (s: String, x: Int, y: Int) => {
        Thread.sleep(10)
        "%s = %d".format(s, x + y)
      }
      
      val res = benchmark3(f, ("sum", 18, 12))
      val tenMs: Double = 1e7
      res.actionStats.mean must beCloseTo(tenMs, 3 * res.actionStats.sd)
    }
    
    "generate statistics for fonctions of three parameters randomly chosen" in {
      import implicits.toRandomGenerator
      
      val f = (s: String, x: Int, y: Int) => {
        Thread.sleep(10)
        "%s = %d".format(s, x + y)
      }
      
      val res = benchmark3(f, Array(("sum", 18, 12), ("res", 3, 7), ("value", 2, 8)))
      val tenMs: Double = 1e7
      res.actionStats.mean must beCloseTo(tenMs, 3 * res.actionStats.sd)
    }
    
    "generate statistics for fonctions of three parameters using a custom generator" in {
      import implicits.toCustomGenerator
      
      val f = (s: String, x: Int, y: Int) => {
        Thread.sleep(10)
        "%s = %d".format(s, x + y)
      }
      
      var x = 0
      def y = (x * 2) % 3
      
      val gen = () => { x = x + 1; ("sum", x, y) }
      
      val res = benchmark3(f, gen)
      val tenMs: Double = 1e7
      res.actionStats.mean must beCloseTo(tenMs, 3 * res.actionStats.sd)
    }
  }
}
