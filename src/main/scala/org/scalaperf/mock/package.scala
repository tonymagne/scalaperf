package org.scalaperf

import java.util.WeakHashMap
import generator.{Generator, CustomGen, ValueGen, Gen}

package object mock {
  private val allInvocations = new WeakHashMap[String, Map[String, Generator[java.lang.Object]]]
  
  private def get(objectClassName: String): Option[Map[String, Generator[java.lang.Object]]] = 
    if (allInvocations.get(objectClassName) != null) Some(allInvocations.get(objectClassName))
    else None

  
  def mock[T](implicit m: scala.reflect.Manifest[T]): T = {
    import java.lang.reflect.{InvocationHandler, Method, Proxy}
    val handler = new InvocationHandler {
      
      override def invoke(proxy: Object, method: Method, args: Array[Object]): Object = {
        method.getName match {
          case "toString" =>  "Dynamic Proxy mock[" + m.erasure + "]"
          case _ => {
            val result = get(proxy.getClass.getName) match {
              case Some(m) => m.get(method.getName)
              case None    => None
            }
            result match {
              case Some(o) => o.sample
              case None    => 
                throw new UnsupportedOperationException("Mock not configured for method %s of class %s.".format(method.getName, proxy.getClass.getInterfaces.map(_.getName).mkString(", ")))
            }
          }
        }
      }
      
    }
    
    val proxy = Proxy.newProxyInstance(m.erasure.getClassLoader, Array(m.erasure), handler).asInstanceOf[T]
    allInvocations.put(proxy.asInstanceOf[java.lang.Object].getClass.getName, Map())
    proxy
  }

  implicit def funcToStubbed[T](c: => T) = new Stubbed(c)
  
  def any[A]: A = null.asInstanceOf[A]
  
  private[mock] class Stubbed[T](c: => T) {

    def uses(next: () => T): Unit = extractMethod(c) match {
      case Some((className, methodName)) => addInvocation(className, methodName, Gen.custom(next))
      case _ => ()
    }
    
    def returns(v: T): Unit = extractMethod(c) match {
      case Some((className, methodName)) => addInvocation(className, methodName, Gen.value(v))
      case _ => ()
    }
    
    private def extractMethod(c: => T): Option[(String, String)] = try {
      c
      None
    } catch {
      case e: UnsupportedOperationException => 
        def isProxy(ste: StackTraceElement) = ste.getClassName startsWith "$Proxy"
        val ste = e.getStackTrace.filter(isProxy _).head
        Some(ste.getClassName, ste.getMethodName)
    }
    
    private def addInvocation(className: String, methodName: String, gen: Generator[T]): Unit = {
      get(className) match {
        case Some(invocations) =>
          val newInvocations = invocations + (methodName -> gen.asInstanceOf[Generator[java.lang.Object]])
          allInvocations.put(className, newInvocations)
        case None => ()
      }
    }
    
  }
}
