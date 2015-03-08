//val s = (1 to 10).map(x => {println(s"Running $x");(1 to 1000000).toList})

import scala.concurrent.forkjoin.ForkJoinPool
import scala.concurrent.{ExecutionContext, Future, Promise}
implicit val ec = ExecutionContext.fromExecutorService(new ForkJoinPool(2))

val f  = (1 to 120).map(x => {
  val p = Promise[List[Int]]

  Future {
    println(s"Running $x")
    p.success((1 to 100000).toList)
    println(s"Done $x")
  }
  p.future.onSuccess{case y => println(s"$x => ${y.sum}")}
}
)

//val ff = Future.sequence(f)

//val fResult = Await.result(ff, 30 seconds)


Thread.sleep(10000)

//fResult.foreach(x => println(x.sum))
// val ff = Future.sequence(f)
// ec.shutdown

//implicit val ec = ExecutionContext.fromExecutorService(new ForkJoinPool(2))

// val g  = (1 to 30).map(x => {println(s"Running $x");Future{(1 to 100000).toList}})

/*
Future.sequence(g).onComplete {
    case Success(x) => x.foreach(y => println(y.sum))
    case Failure(x) => x.printStackTrace
}
*/
// val gf = Future.sequence(g)

// val fResult = Await.result(ff, 5 minutes)
// println(fResult)
//val gResult = Await.result(gf, 5 minutes)

//ec.shutdown

//(1 to 10).map(x => Future{(1 to 1000000).toList.reduce(_+_)}).foreach(_.onSuccess{case x => println(x)})
