package org.scalaperf
package format

import scala.annotation.tailrec
import scala.math.{min, max}
import bench.BenchResult
import statistics.{Stats, Outliers}

class HumanFormatter(result: BenchResult) {
  import HumanFormatter.{humanString, scientificString}
  
  def humanFormat = (statsInfo(humanString) ++
                     cleanIssuesInfo() ++
                     environmentalNoiseIssuesInfo() ++
                     sdIssuesForActionInfo() ++
                     outliersIssuesInfo() ++
                     serialCorrelationIssuesInfo()).mkString("\n")
  
  def humanFormatFull = (statsInfo(humanString) ++
                         cleanIssuesFullInfo() ++
                         environmentalNoiseIssuesFullInfo() ++
                         sdIssuesForActionFullInfo() ++
                         outliersIssuesFullInfo(humanString) ++
                         serialCorrelationIssuesFullInfo()).mkString("\n")
  
  def scientificFormat = (statsInfo(scientificString) ++
                          cleanIssuesInfo() ++
                          environmentalNoiseIssuesInfo() ++
                          sdIssuesForActionInfo() ++
                          outliersIssuesInfo() ++
                          serialCorrelationIssuesInfo()).mkString("\n")
  
  def scientificFormatFull = (statsInfo(scientificString) ++
                              cleanIssuesFullInfo() ++
                              environmentalNoiseIssuesFullInfo() ++
                              sdIssuesForActionFullInfo() ++
                              outliersIssuesFullInfo(scientificString) ++
                              serialCorrelationIssuesFullInfo()).mkString("\n")
  
  private def cleanIssuesInfo() = {
    result.cleanFraction.map(_ => Array("- WARNING: Execution time may be inaccurate.")).getOrElse(Array[String]())
  }
  
  private def cleanIssuesFullInfo() = {
    result.cleanFraction.map { d => 
      val cleanPercent = (100 * d).toInt
      Array("- Execution times may be too small.", 
            "  A final JVM cleanup took %s%% of the total execution.".format(cleanPercent),
            "  Execution time too small.  And should be included in the measurements.")
    }.getOrElse(Array[String]())
  }
  
  private def sdIssuesForActionInfo() = {
    result.fractionVarOutlierMin.map(_ => Array("- WARNING: SD VALUES MAY BE INACCURATE (action statistics).")).getOrElse(Array[String]())
  }
  
  private def sdIssuesForActionFullInfo() = {
    def inflated(d: Double): String = {
      d match {
        case _ if d < 0.10 => "might be somewhat inflated"
        case _ if d < 0.50 => "likely INFLATED"
        case _             => "ALMOST CERTAINLY GROSSLY INFLATED"
      }
    }
    
    result.fractionVarOutlierMin.map { d =>
      val fractionPercent = (100 * d).toInt
      Array("- action SD values %s by outliers".format(inflated(d)),
            "  they cause at least %d%% of the measured VARIANCE according to a equi-valued outlier model".format(fractionPercent))
    }.getOrElse(Array[String]())
  }
  
  private def outliersIssuesInfo() = {
    val hasExtreme = result.outliers.highExtreme.nonEmpty || result.outliers.lowExtreme.nonEmpty
    val hasMild = result.outliers.highMild.nonEmpty || result.outliers.lowMild.nonEmpty
    if (hasExtreme)   Array("- WARNING: execution times have EXTREME OUTLIERS.")
    else if (hasMild) Array("- WARNING: execution times have mild outliers.")
    else              Array[String]()
  }
  
  private def outliersIssuesFullInfo(formatter: Double => String) = {
    val ols = result.outliers
    def header(): Option[String] = {
      if (ols.isEmpty) None 
      else {
        val header = "- EXECUTION TIMES APPEAR TO HAVE OUTLIERS (determined using median = %s, interquantile range = %s)".format(formatter(ols.median), formatter(ols.iqr))
        Some(header)
      }
    }
    
    def outliersItemToString(ols: List[(Double, Int)], s1: String, s2: String): Option[String] = {
      if (ols.isEmpty) None 
      else {
        val info = "  %d %s %s (on the %s side): %s".format(
          ols.size,
          if (ols.size == 1) "is" else "are",
          s1,
          s2,
          ols.map(o => "#%d = %s".format(o._2, formatter(o._1))).mkString(", "))
        Some(info)
      }
    }
    
    Array(header(),
          outliersItemToString(ols.lowExtreme,  "EXTREME", "low"),
          outliersItemToString(ols.lowMild,     "mild",    "low"),
          outliersItemToString(ols.highExtreme, "EXTREME", "high"),
          outliersItemToString(ols.highMild,    "mild",    "high")).filter(_.isDefined).map(_.get)
  }
  
  private def serialCorrelationIssuesInfo() = {
    val sc = result.serialCorrelation
    if (sc.hasMoreThanExpected) Array("- WARNING: execution times may have serial correlation.")
    else                        Array[String]()
  }
  
  private def serialCorrelationIssuesFullInfo() = {
    val sc = result.serialCorrelation
    def rToOptionString(os: List[(Double, Double, Int)], side: String): Option[String] = {
      if (os.isEmpty) None
      else {
        val info = "  %s".format(os.map(o => "r(%s) is %s sigma %s its mean".format(o._3, o._2, side)).mkString(", "))
        Some(info)
      }
    }
    
    if (sc.isMeaningless) {
      Array("- UNKNOWN IF EXECUTION TIMES HAVE SERIAL CORRELATION OR NOT: N = %d < 50 is too small for autocorrelation tests.".format(sc.N))
    } else if (sc.hasMoreThanExpected) {
      Array(Some("- EXECUTION TIMES HAVE SERIAL CORRELATION"),
            Some("  %s of the %s autocorrelation function coefficients (r[k]) that were computed are expected to fall outside their 95%% CI".format(sc.nbOutsideExpected, sc.K)),
            Some("  but found:"), 
            rToOptionString(sc.outsideBelow, "below"),
            rToOptionString(sc.outsideAbove, "above"),
            Some("  the 95% CI for the r[k] was calculated as mean +- 1.96 sigma (i.e. a Gaussian distribution was assumed)")).filter(_.isDefined).map(_.get)
    } else {
      Array[String]()
    }
  }
  
  private def environmentalNoiseIssuesInfo() = {
    result.environmentalNoise match {
      case Some(_) => Array("- WARNING: SD VALUES MAY BE INACCURATE (noise).")
      case None    => Array("- WARNING: SD results have unknown validity (the environmental noise test was skipped)")
    }
  }
  
  private def environmentalNoiseIssuesFullInfo() = {
    result.environmentalNoise match {
      case Some(sdFraction) =>
        Array("- Block SD values MAY NOT REFLECT TASK'S INTRINSIC VARIATION",
              "  guesstimate: environmental noise explains at least %s%%  of the measured SD".format(100 * sdFraction.toInt))
      case None             =>
        Array("- SD results have unknown validity (the environmental noise test was skipped)")
    }
  }
  
  private def statsInfo(formatter: Double => String) = {
    val stats = result.actionStats
    Array("First = %s".format(formatter(stats.first)),
          "Mean = %s, %s".format(formatter(stats.mean), formatCi(stats.mean, stats.meanLower, stats.meanUpper, formatter)),
          "SD = %s, %s".format(formatter(stats.sd), formatCi(stats.sd, stats.sdLower, stats.sdUpper, formatter)),
          "%sth = %s".format(stats.percentile.p, formatter(stats.percentile.value)))
  }
  
  private def formatCi(d: Double, lower: Double, upper: Double, formatter: Double => String): String = {
    def computeSymmetry: (Boolean, Double, Double) =  {
      val diffLower = d - lower
      val diffUpper = upper - d
      val diffMax = max(diffLower, diffUpper)
      val diffMin = min(diffLower, diffUpper)
      val asymetry = (diffMax - diffMin) / diffMin
      (asymetry <= 1e-3, diffLower, diffUpper)
    }
    
    def isInsideInterval: Boolean = (lower <= d) && (d <= upper)
    
    if (isInsideInterval) {
      val (symmetric, diffLower, diffUpper) = computeSymmetry
      if (symmetric) "(CI deltas: +- %s)".format(formatter(diffLower))
      else           "(CI deltas: -%s, +%s)".format(formatter(diffLower), formatter(diffUpper))
    } else           "(CI: [%s, %s])".format(formatter(lower), formatter(upper))
  }
}

private[format] object HumanFormatter {
  type Day    = Long
  type Hour   = Long
  type Minute = Long
  type Second = Long
  type Millis = Long
  type Micros = Long
  type Nanos  = Long
  type Picos  = Long
  
  private def parse(duration: Double): (Day, Hour, Minute, Second, Millis, Micros, Nanos, Picos) = {
    @tailrec
    def parseTr(current: (Day, Hour, Minute, Second, Millis, Micros, Nanos, Picos)): (Day, Hour, Minute, Second, Millis, Micros, Nanos, Picos) = {
      current match {
        case (_, _, _, _, _, _, _, picos)                   if picos  >= 1000 => parseTr(((0, 0, 0, 0, 0, 0, picos / 1000, picos % 1000)))
        case (_, _, _, _, _, _, nanos, picos)               if nanos  >= 1000 => parseTr((0, 0, 0, 0, 0, nanos / 1000, nanos % 1000, picos))
        case (_, _, _, _, _, micros, nanos, picos)          if micros >= 1000 => parseTr((0, 0, 0, 0, micros / 1000, micros % 1000, nanos, picos))
        case (_, _, _, _, millis, micros, nanos, picos)     if millis >= 1000 => parseTr((0, 0, 0, millis / 1000, millis % 1000, micros, nanos, picos))
        case (_, _, _, sec, millis, micros, nanos, picos)   if sec >= 60      => parseTr((0, 0, sec / 60, sec % 60, millis, micros, nanos, picos))
        case (_, _, min, sec, millis, micros, nanos, picos) if min >= 60      => parseTr((0, min / 60, min % 60, sec, millis, micros, nanos, picos))
        case (d, h, min, sec, millis, micros, nanos, picos) if h   >= 24      => parseTr((d + h / 24, h % 24, min, sec, millis, micros, nanos, picos))
        case _                                                                => current
      }
    }
    parseTr((0, 0, 0, 0, 0, 0, 0, duration.toLong))
  }
  
  def humanString(duration: Double): String = {
    val durationInPs = duration * 1000
    parse(durationInPs) match {
      case (0, 0,   0,   0,      0,      0,     0,     0) => "0"
      case (d, h, min, sec, millis, micros, nanos, picos) => 
        Array(durationToPluralString(d,      "day"),
              durationToPluralString(h,      "hour"),
              durationToString      (min,    "min"),
              durationToString      (sec,    "s"),
              durationToString      (millis, "ms"),
              durationToString      (micros, "\u00B5s"),
              durationToString      (nanos,  "ns"),
              durationToString      (picos,  "ps")).filter(_ != "").mkString(" ")
    }
  }
  
  private[format] def durationToString(duration: Long, u: String)       = if (duration != 0) "%d%s".format(duration, u) else ""
  private[format] def durationToPluralString(duration: Long, u: String) = if (duration != 0) "%d%s".format(duration, "%s%s".format(u, if (duration > 1) "s" else "")) else ""
  private[format] def durationToDecimalString(a: Long, b: Long, u: String) = if (b != 0) "%d.%03d%s".format(a, b, u) else "%d%s".format(a, u)
  
  def scientificString(duration: Double): String = {
    val durationInPs = duration * 1000
    parse(durationInPs) match {
      case (0, 0,   0,   0,      0,      0,     0,     0) => "0"
      case (0, 0,   0,   0,      0,      0,     0, picos) => durationToString(picos, "ps")
      case (0, 0,   0,   0,      0,      0, nanos, picos) => durationToDecimalString(nanos, picos, "ns")
      case (0, 0,   0,   0,      0, micros, nanos,     _) => durationToDecimalString(micros, nanos, "\u00B5s")
      case (0, 0,   0,   0, millis, micros,     _,     _) => durationToDecimalString(millis, micros, "ms")
      case (0, 0,   0, sec, millis,      _,     _,     _) => durationToDecimalString(sec, millis, "s")
      case (d, h, min, sec, millis,      _,     _,     _) => 
        Array(durationToPluralString(d,      "day"),
              durationToPluralString(h,      "hour"),
              durationToString      (min,    "min"),
              if (sec != 0 || millis != 0) durationToDecimalString(sec, millis, "s") else "").filter(_ != "").mkString(" ")
    }
  }
}
