# Scalaperf #

scalaperf is a tool allowing to benchmark Scala code.  
It is inspired by Brent Boyer's articles on Java benchmarking:  
[Robust Java benchmarking, Part 1: Issues](http://www.ibm.com/developerworks/java/library/j-benchmark1/index.html)  
[Robust Java benchmarking, Part 2: Statistics and solutions](http://www.ibm.com/developerworks/java/library/j-benchmark2/index.html)  
[Java benchmarking article](http://www.ellipticgroup.com/html/benchmarkingArticle.html)

#### Other resources ####
[http://wikis.sun.com/display/HotSpotInternals/MicroBenchmarks](http://wikis.sun.com/display/HotSpotInternals/MicroBenchmarks)  
[http://www.ibm.com/developerworks/java/library/j-jtp02225/index.html](http://www.ibm.com/developerworks/java/library/j-jtp02225/index.html)

### Features ###
- allows to benchmark function of 1 parameter to 5 parameters
- uses parameter generators allowing to control the parameters provided at each execution
  - value generator: the same value is provided each time
  - random generator: a value is randomly selected in the provided array
  - custom generator: a function returning the next value
- computes:
  - mean with confidence interval
  - standard deviation with confidence interval
  - the 99th percentile
  - statistical analysis (outliers, serial correlation)
- provides a mocking facility allowing to mock external dependency (i.e. DB) while performing a benchmark  
  (with minimum overhead, a few nanoseconds)
- allows to measure the throughput of a function


### Example 1 ###
Benchmarks the fibonacci function using the default configuration (10 second warmup, 60 measurements)  

    import org.scalaperf.bench.{benchmark1, scalaperfInfo}
    import org.scalaperf.implicits.{toValueGenerator, toHumanFormat}
    
    def fibonacci(n: Int): Long = n match {
      case 0 => 0
      case 1 => 1
      case _ => fibonacci(n - 1) + fibonacci(n - 2)
    }
    
    println(scalaperfInfo("fibonnaci"))
    val res = benchmark1(fibonacci _, 35)
    println(res.scientificFormatFull)

###### Figures ######

    Scala Perf: fibonnaci
    Windows 7 (version 6.1)
    Intel64 Family 6 Model 30 Stepping 5, GenuineIntel (8 cores)
    Java(TM) SE Runtime Environment (build 1.6.0_18-b07)
    Java HotSpot(TM) 64-Bit Server VM (build 16.0-b13, mixed mode)
    First = 103.985ms
    Mean = 129.449ms, (CI deltas: -5.034ms, +5.048ms)
    SD = 56.842ms, (CI deltas: -9.205ms, +6.107ms)
    99th = 169.545ms
    - SD results have unknown validity (the environmental noise test was skipped)
    
### Example 2 ###
 Benchmarks the fibonacci function (memoized) with log enabled. 
 
    import org.scalaperf.bench.{benchmark1, scalaperfInfo}
    import org.scalaperf.implicits.{toValueGenerator, toHumanFormat}
    
    def memoizedFibonacci(n: Int): Long = {
      def memoizedFibonacciTr(k: Int, fkMinusOne: Long, fkMinusTwo: Long): Long = {
        if (k <= 1) fkMinusOne
        else memoizedFibonacciTr(k - 1, fkMinusOne + fkMinusTwo, fkMinusOne)
      }
      memoizedFibonacciTr(n, 1, 0)
    }
  
    implicit val config = new BenchConfig {
      override def logEnabled = true
    }
    
    println(scalaperfInfo("memoized fibonnaci"))
    val res = benchmark1(memoizedFibonacci _, 35)
    println(res.scientificFormatFull)

###### Figures ######

    Scala Perf: memoized fibonnaci
    Windows 7 (version 6.1)
    Intel64 Family 6 Model 30 Stepping 5, GenuineIntel (8 cores)
    Java(TM) SE Runtime Environment (build 1.6.0_18-b07)
    Java HotSpot(TM) 64-Bit Server VM (build 16.0-b13, mixed mode)
    First = 16.679µs
    Mean = 42.668ns, (CI deltas: -1.319ns, +1.332ns)
    SD = 30.543µs, (CI deltas: -5.659µs, +4.082µs)
    99th = 53.925ns
    - SD results have unknown validity (the environmental noise test was skipped)
    - action SD values ALMOST CERTAINLY GROSSLY INFLATED by outliers
      they cause at least 99% of the measured VARIANCE according to a equi-valued outlier model

### Example 3 ###
Benchmarks the fibonacci function (fork/join) with log enabled.  

    import org.scalaperf.bench.{benchmark1, scalaperfInfo}
    import org.scalaperf.implicits.{toValueGenerator, toHumanFormat}
    import scala.concurrent.forkjoin.{RecursiveTask, ForkJoinPool}

    def fibonacci(n: Int): Long = n match {
      case 0 => 0
      case 1 => 1
      case _ => fibonacci(n - 1) + fibonacci(n - 2)
    }
  
    class Fibonacci(n: Int) extends RecursiveTask[Long] { 
      override def compute: Long = {
        if (n <= 16) fibonacci(n)
        else {
          val f1 = new Fibonacci(n - 1)
          val f2 = new Fibonacci(n - 2)
          f1.fork
          f2.fork
          f2.join + f1.join
        }
      }
    }

    val pool = new ForkJoinPool
  
    def fjFibonacci(n: Int): Long = {
      val f = new Fibonacci(n)
      pool.invoke(f)
    }
  
    implicit val config = new BenchConfig {
      override def logEnabled = true
    }
    
    println(scalaperfInfo("fork/foin fibonnaci"))
    val res = benchmark1(fjFibonacci _, 35)
    println(res.scientificFormatFull)

###### Figures ######
    Scala Perf: fork/foin fibonnaci
    Windows 7 (version 6.1)
    Intel64 Family 6 Model 30 Stepping 5, GenuineIntel (8 cores)
    Java(TM) SE Runtime Environment (build 1.6.0_18-b07)
    Java HotSpot(TM) 64-Bit Server VM (build 16.0-b13, mixed mode)
    First = 115.564ms
    Mean = 30.574ms, (CI deltas: -16.132µs, +15.679µs)
    SD = 505.234µs, (CI deltas: -86.380µs, +64.166µs)
    99th = 30.707ms
    - SD results have unknown validity (the environmental noise test was skipped)
    - action SD values might be somewhat inflated by outliers
      they cause at least 1% of the measured VARIANCE according to a equi-valued outlier model

### Example 4 ###

Benchmarks a function of 2 parameter using a random generator.  

    import org.scalaperf.bench.{benchmark2, scalaperfInfo}
    import org.scalaperf.implicits.{toRandomGenerator, toHumanFormat}

    val f = (x: Int, y: Int) => {
      Thread.sleep(10)
      x + y
    }
    
    println(scalaperfInfo("Benchmark using a random generator"))
    val res = benchmark2(f, Array((18, 12), (23, 6), (5, 9), (43, 2), (64, 55)))
    println(res.scientificFormatFull)

### Example 5 ###
Benchmarks the Array.map function using a custom generator.  

    import org.scalaperf.bench.{benchmark1, scalaperfInfo}
    import org.scalaperf.implicits.{toCustomGenerator, toHumanFormat}

    val array = new Array[Int](1024 * 1024)
    
    val arrayGenerator = () => array.map(_ => Random.nextInt(100))
    
    implicit val config = new BenchConfig {
      override def logEnabled = true
    }
    
    println(scalaperfInfo("Array.map"))
    val res = benchmark1((a: Array[Int]) => a.map(_ * 2), arrayGenerator)
    println(res.scientificFormatFull)

###### Figures ######
    Scala Perf: Array.map
    Windows 7 (version 6.1)
    Intel64 Family 6 Model 30 Stepping 5, GenuineIntel (8 cores)
    Java(TM) SE Runtime Environment (build 1.6.0_18-b07)
    Java HotSpot(TM) 64-Bit Server VM (build 16.0-b13, mixed mode)
    First = 50.377ms
    Mean = 45.484ms, (CI deltas: -1.811ms, +1.805ms)
    SD = 40.781ms, (CI deltas: -7.112ms, +5.054ms)
    99th = 60.181ms
    - SD results have unknown validity (the environmental noise test was skipped)
    - action SD values ALMOST CERTAINLY GROSSLY INFLATED by outliers
      they cause at least 76% of the measured VARIANCE according to a equi-valued outlier model

### Example 6 ###
Benchmarks Scala 2.9.0 ParArray.map function using a custom generator.  

    val array = new Array[Int](1024 * 1024)
    
    val arrayGenerator = () => array.map(_ => Random.nextInt(100)).par
    
    implicit val config = new BenchConfig {
      override def logEnabled = true
    }
    
    println(scalaperfInfo("ParArray.map"))
    val res = benchmark1((a: ParArray[Int]) => a.map(_ * 2), arrayGenerator)
    println(res.scientificFormatFull)

###### Figures ######
    Scala Perf: ParArray.map
    Windows 7 (version 6.1)
    Intel64 Family 6 Model 30 Stepping 5, GenuineIntel (8 cores)
    Java(TM) SE Runtime Environment (build 1.6.0_18-b07)
    Java HotSpot(TM) 64-Bit Server VM (build 16.0-b13, mixed mode)
    First = 174.377ms
    Mean = 9.290ms, (CI deltas: -122.857µs, +129.066µs)
    SD = 5.676ms, (CI deltas: -1.113ms, +826.174µs)
    99th = 10.480ms
    - SD results have unknown validity (the environmental noise test was skipped)
    - action SD values ALMOST CERTAINLY GROSSLY INFLATED by outliers
      they cause at least 57% of the measured VARIANCE according to a equi-valued outlier model

### Caution  ###
The above figures are only given for illustration.  
All benchmark results are dependent of the hardware and software infrastructure in which they are performed.  
It is strongly recommended that you run your benchmark on as many platforms as possible before forming an opinion.  
Your ability to doubt about the results is always your best tool.   
Benchmarks should be used with caution. Their results can easily be misleading.   
They give very little imformation about the code in production mode.  
You should always concentrate on producing software that works using good programming practices, unit tests and code reviews.  
Benchmarks should be used ultimately, in order to compare two implementations when all other criteria have been considered.
