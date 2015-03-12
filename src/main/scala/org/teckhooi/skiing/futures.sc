// it worked because the count is small i.e. 2 counts and it is a single threaded task
(1 to 2).map(x => (1 to 1000000).map(_.toLong).toList).map(_.sum).sum

// single thread task crashed because it ran out of memory even though the count is increased slightly.
// This is also highly dependent on the memory allocated to the runtime JVM
(1 to 4).map(x => (1 to 1000000).map(_.toLong).toList).map(_.sum).sum

import scala.concurrent.duration._
import scala.concurrent.forkjoin.ForkJoinPool
import scala.concurrent.{Await, ExecutionContext, Future}
// implicit val ec = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(4))
implicit val ec = ExecutionContext.fromExecutorService(new ForkJoinPool(4))
//implicit val ec = ExecutionContext.fromExecutorService(new ForkJoinPool(2))
//implicit val ec = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(2))

// multi threaded task crashed because it ran out of memory
def runmeCrashed(n: Int = 120) = (1 to n).map{tuple =>
  Future {
    (1 to 100000).map(_.toLong).toList
  }}
val bad = Await.result(Future.sequence(runmeCrashed()), 10 second)
bad.reduce(_.sum + _.sum)

// using Future to run each group concurrently. Use small memory footprint.
def runme(n: Int = 120) = (1 to n).grouped(2).flatMap{tuple =>
    val batches = Future.sequence(tuple.map{x => Future {
        (1 to 100000).map(_.toLong).toList
    }})
    Await.result(batches, 10 second).map(_.sum)
}
val result = runme().toList   // because iterator would not reset its pointer after it has iterated thru its elements
println(s"The total of size ${result.size}  is " + (if (result.sum != 600006000000L) "NOT " else "") + "tally with 600006000000")
ec.shutdown
// do not use List if it is intended for parallel executions. Use Vector or Seq i.e. toVector or toSeq but not toList.
// Using parallel collection to run each group concurrently. Use small memory footprint.
def runmePar(n: Int = 120) = (1 to n).grouped(2).toVector.flatMap{tuple =>
  tuple.par.map{x => {
      (1 to 100000).map(_.toLong).toList
  }}.map(_.sum)
}
val resultPar = runmePar()
println(s"The total of size ${resultPar.size}  is " + (if (resultPar.sum != 600006000000L) "NOT " else "") + "tally with 600006000000")
