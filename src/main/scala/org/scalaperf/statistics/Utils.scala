package org.scalaperf
package statistics

import java.util.concurrent.atomic.AtomicLong
import java.security.{SecureRandom, MessageDigest}
import java.io.{DataInputStream, ByteArrayInputStream}

object Utils {
  val id = new AtomicLong()
  
  def makeSeed(): Long = {
    val seedPossiblyHighQuality = try {
      makeSeedSecure()
    } catch {
      case t: Throwable => 0L
    }
    seedPossiblyHighQuality ^ makeSeedUnique()
  }
  
  private def makeSeedSecure(): Long =  {
    val random = SecureRandom.getInstance("SHA1PRNG")
    new DataInputStream(new ByteArrayInputStream(random.generateSeed(8))).readLong
  }
  
  private def makeSeedUnique(): Long = {
    val hashSerialNumber: Long = hash(id.incrementAndGet())
    val hashTime: Long = hash(System.nanoTime())
    
    val bitsHigh = (hashSerialNumber << 32)
    val bitsLow  = hashTime
    bitsHigh | bitsLow
  }
  
  /////////////////////////////////////////////////////////
  // Scala translation of Brent Boyer code (bb.util.HashUtil)
  // http://www.ellipticgroup.com/html/benchmarkingArticle.html
  /////////////////////////////////////////////////////////
  
  private val prime1 = 1491735241
  
  private val prime2 = 2041543619
  
  private def enhance(h: Int) = {
    try {
      val md = MessageDigest.getInstance("SHA-1");
      md.update( (h >>> 24).toByte )
      md.update( (h >>> 16).toByte )
      md.update( (h >>>  8).toByte )
      md.update( (h >>>  0).toByte )
      val digest = md.digest()
      var digestAsInt = 0
      for (i <- 0 until digest.length) {
        val shift = (i % 4) * 8
        digestAsInt ^= ((digest(i) & 0xFF) << shift)
      }
      digestAsInt
    } catch {
      case e: Exception => enhanceFallback4(h)
    }
  }
  
  private def enhanceFallback1(in: Int): Int = {
    var h = in
    h += ~(h << 9)
    h ^=  (h >>> 14)
    h +=  (h << 4)
    h ^=  (h >>> 10)
    h
  }
  
  private def enhanceFallback2(in: Int): Int = {
    var h = in
    h ^= (h >>> 11)
    h ^= (h << 7) & 0x9D2C5680
    h ^= (h << 15) & 0xEFC60000
    h ^= (h >>> 18)
    h
  }
  
  private def enhanceFallback3(h: Int): Int = (prime1 * h) + prime2
  
  private def enhanceFallback4(h: Int): Int = enhanceFallback2( enhanceFallback3( enhanceFallback1( h ) ) )
  
  def hash(d: Double): Int = enhance(d.hashCode)
  
  def hash(l: Long): Int = enhance(l.hashCode)
  
}