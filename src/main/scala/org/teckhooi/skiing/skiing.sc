import java.util.concurrent.Executors

import scala.concurrent.Await
import scala.io.Source
val expectedMap = List(List(4, 8, 7, 3),
  List(2, 5, 9, 3),
  List(6, 3, 2, 5),
  List(4, 4, 1, 6))
val textSlopes = Source.fromFile("/home/thlim/workspace/playground/coffee-break/src/main/resources/small-map.txt").getLines().drop(1).toList
val slopes = textSlopes.map(_.split(' ').map(_.toInt).toList)
slopes.size

type Point = Tuple3[Int, Int, Int]

class Navigation(val peaks: List[List[Int]], val maxY: Int, val maxX: Int) {
  val movements = List((1, 0), (0, 1), (-1, 0), (0, -1))

  def newLoc(y: Int, x: Int, height: Int) = {
    val moves = movements.map(d => (d._1 + y, d._2 + x)).filter(d => d._1 >= 0 && d._1 < maxY && d._2 >= 0 && d._2 < maxX)

    moves.flatMap{case p =>
      val peak = peaks(p._1 % maxY)(p._2 % maxX)
      if (peak < height) Some(p._1, p._2, peak) else None
    }
  }
}
val smallMapNavi = new Navigation(expectedMap, 4, 4)
smallMapNavi.newLoc(1, 1, smallMapNavi.peaks(1)(1))

def skiing(y: Int, x: Int, navi: Navigation): List[Int] = {
  def _skiing(paths: List[Point], queue: List[Point], acc: List[Int]): List[Int] = {
    paths match {
      case Nil => maxRoute(navi.peaks(y)(x) :: queue.reverse.map(_._3), acc)
      case z :: zs => {
        val localAcc = _skiing(navi.newLoc(z._1, z._2, navi.peaks(z._1)(z._2)), z :: queue, acc)
        if (zs.isEmpty) localAcc else _skiing(zs, queue, localAcc)
      }
    }
  }

  _skiing(navi.newLoc(y, x, navi.peaks(y)(x)), List.empty[Point], List.empty[Int])
}
skiing(1, 1, smallMapNavi)
skiing(0, 1, smallMapNavi)

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

val executorService = Executors.newFixedThreadPool(16)
implicit val ec = ExecutionContext.fromExecutorService(executorService)
def futureSkiing(navi: Navigation): List[Future[List[Int]]] = {
  def _futureSkiing(y: Int, x: Int, acc: List[Future[List[Int]]]) : List[Future[List[Int]]] = {
    val f = Future {
      skiing(y, x, navi)
    }
    if (y + 1 == navi.maxY && x + 1 == navi.maxX) f :: acc
    else if (x + 1 >= navi.maxX) _futureSkiing(y + 1, 0, f :: acc)
    else _futureSkiing(y, x + 1, f :: acc)
  }

  _futureSkiing(0, 0, List.empty[Future[List[Int]]])
}
def maxRoute(x: List[Int], y: List[Int]) = {
  def heightDiff(xs: List[Int]) = {
    xs.head - xs.last
  }
  if (x.length > y.length || (heightDiff(x) > heightDiff(y) && x.length == y.length)) x else y
}
val bigMap = Source.fromFile("/home/thlim/workspace/playground/coffee-break/src/main/resources/map.txt").getLines().drop(1).toList
val bigSlopes = bigMap.map(_.split(' ').toList.map(_.toInt))
val bigMapNavi = new Navigation(bigSlopes,  bigSlopes.size,  bigSlopes.size)
val result = Await.result(Future.sequence(futureSkiing(bigMapNavi)), 5 minute)
val selectedPath = result.reduce((a, b) => maxRoute(a, b))
println(s"${selectedPath.head - selectedPath.last} :: ${selectedPath.size}")
ec.shutdown()
