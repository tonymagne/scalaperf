package org.scalaperf
package throughput

import org.specs2.mutable._
import implicits.toValueGenerator

object ThroughputSpecs extends Specification {
  def myFunction(name: String): String = {
    Thread.sleep(10)
    name
  }
  

  "The Throughput object" should {
    "record the number of call per second for a function" in {
      val count = throughput1(myFunction _, "Tony")
      
      println("Throughput = " + count)
      count must beLessThan(110d)
    }
  }

    
  "The Throughput actor" should {
    "record the number of call per second for a function" in {
      val counter = new ThroughputCounter
      counter.start
      for (i <- 0 until 6000) {
        myFunction("Tony")
        counter ! Hit
      }
      counter ! Stop
      println("Throughput = " + counter.get)
      counter.get must beLessThan(110d)
    } 
  }

}
