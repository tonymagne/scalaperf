package org.scalaperf
package mock

import org.specs2.mutable._

object MockerSpecs extends Specification {
  sequential
  
  trait ATrait {
    val aIntVal = 1
    val aStringVal = "string"
    def anAbstractMethod(arg: String): Int
    def aConcreteMethod(arg1: String, arg2: Int): String = arg1 + arg2
  }
  
  "The Mocker object" should {  
    "create a mock object for a given Trait" in {
      val m = mock[ATrait]
      //m should haveSuperclass[ATrait]
      
      val haveSuperclass = m.isInstanceOf[ATrait]
      haveSuperclass must beTrue
    }
    
    "allow to return the same value for a method call" in {
      val m = mock[ATrait]
      m.anAbstractMethod(any[String]) returns 2

      m.anAbstractMethod("Tony") must beEqualTo(2)
      m.anAbstractMethod("ynoT") must beEqualTo(2)
      m.anAbstractMethod("nyTo") must beEqualTo(2)
    }
    
    "allow to mock a concreate method" in {
      val m = mock[ATrait]
      m.aConcreteMethod(any[String], any[Int]) returns "success"
      
      m.aConcreteMethod("hello", 3) must beEqualTo("success")
    }
    
    "allow to use a custom generator for a method call" in {
      val m = mock[ATrait]
      var i = 0
      m.anAbstractMethod(any[String]) uses(() => {i = i + 1; i})

      m.anAbstractMethod("ynoT") must beEqualTo(1)
      m.anAbstractMethod("ynoT") must beEqualTo(2)
      m.anAbstractMethod("ynoT") must beEqualTo(3)
    }
  }
  
  "The mock object" should {
    import bench.{BenchConfig, benchmark1}
    import implicits.{toValueGenerator, toHumanFormat}
    
    "introduce a minimal overhead" in {
      val m = mock[ATrait]
      m.anAbstractMethod(any[String]) returns 2
      
      val res = benchmark1(m.anAbstractMethod, "Tony")
      
      res.actionStats.mean must beCloseTo(100, 3 * res.actionStats.sd)
      println(res.scientificFormat)
      success
    }
  }
}
