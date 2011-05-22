package org.scalaperf
package utils

import org.specs2.mutable._

object MemorySpecs extends Specification {
  
  "The memory object" should {
    "retrive the memory amount used" in {
      Memory.used must beGreaterThanOrEqualTo(0L)
    }
    
    "provide a snapshot of the memory" in {
      val snapshot = Memory.snapshot
      snapshot.total must beGreaterThan(0L)
      snapshot.max must beGreaterThan(0L)
      snapshot.free must beGreaterThanOrEqualTo(0L)
      snapshot.available must beLessThan(snapshot.max)
      snapshot.used must beLessThan(snapshot.total)
    }
    
    
//    "restore a stable situation for the Memory (finalisation an gc) in less than 100 attempts" in {
//      val attempt = Memory.restore
//      // TODO Implement: Use ScalaCheck in order to generate (None, Some(0), ..., Some(100)) 
//    }
    
  }
  
  "The MemorySnapshot class" should {
    "throw an execption when negative values are provided" in {
      MemorySnapshot(-10, 11, 12) must throwA[IllegalArgumentException]
      MemorySnapshot(10, -11, 12) must throwA[IllegalArgumentException]
      MemorySnapshot(10, 11, -12) must throwA[IllegalArgumentException]
      MemorySnapshot(0, 0, 0) must throwA[IllegalArgumentException]
    }
  }

}
