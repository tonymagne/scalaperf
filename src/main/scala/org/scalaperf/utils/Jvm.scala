package org.scalaperf
package utils

import java.lang.management.ManagementFactory

object Jvm {
  def state: JvmState = {
    val classloadingbean = ManagementFactory.getClassLoadingMXBean
    val compileBean = ManagementFactory.getCompilationMXBean
    JvmState(classloadingbean.getTotalLoadedClassCount, 
             classloadingbean.getUnloadedClassCount, 
             if (compileBean.isCompilationTimeMonitoringSupported) Some(compileBean.getTotalCompilationTime) 
             else None)
  }
  
  def diff(initial: JvmState, current: JvmState): Option[String] = {
    val JvmState(ilc, iuc, ict) = initial
    val JvmState(clc, cuc, cct) = current
    val info = Array(
      if (ilc < clc)      "classes loaded" 
      else if (ilc > clc) "class load count DECREASED by %s (IS THIS AN ERROR?)".format(ilc - clc) 
      else                "",
      if (iuc < cuc)      "classes unloaded" 
      else if (iuc > cuc) "class unload count DECREASED by %s (IS THIS AN ERROR?)".format(iuc - cuc) 
      else                "",
      (ict, cct) match {
        case (None, Some(time))                          => "%s ms of compilation occured".format(time)
        case (Some(iTime), Some(cTime)) if iTime < cTime => "%s ms of compilation occured".format(cTime - iTime)
        case (Some(iTime), Some(cTime)) if cTime > iTime => "compilation time DECREASED by %s ms (IS THIS AN ERROR?)".format(iTime - cTime)
        case (Some(time), None)         if time > 0      => "compilation time DECREASED by %s ms (IS THIS AN ERROR?)".format(time)
        case _                                           => ""
      }
    ).filter(_ != "")
    if (info.nonEmpty) Some(info.mkString(", ")) else None
  }
}

case class JvmState(loadedClassCount: Long, unloadedClassCount: Long, totalCompilationTime: Option[Long])
