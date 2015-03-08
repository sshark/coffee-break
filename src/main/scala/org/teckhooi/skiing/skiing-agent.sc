import scala.concurrent.forkjoin.ForkJoinPool
import scala.io.Source

val expectedMap = List(List(4, 8, 7, 3),
  List(2, 5, 9, 3),
  List(6, 3, 2, 5),
  List(4, 4, 1, 6))
val textSlopes = Source.fromFile("/home/thlim/workspace/playground/coffee-break/src/main/resources/small-map.txt").getLines().drop(1).toList
val slopes = textSlopes.map(_.split(' ').map(_.toInt).toList)
slopes.size
case class Point(y: Int, x: Int, height: Int)
class Navigation(val peaks: List[List[Int]], val maxY: Int, val maxX: Int) {
  val movements = List((1, 0), (0, 1), (-1, 0), (0, -1))

  def newLoc(p: Point) = {
    val moves = movements.map(d => (d._1 + p.y, d._2 + p.x))
      .filter(d => d._1 >= 0 && d._1 < maxY && d._2 >= 0 && d._2 < maxX)

    moves.flatMap{case localPoint =>
      val peak = peaks(localPoint._1 % maxY)(localPoint._2 % maxX)
      if (peak < p.height) Some(Point(localPoint._1, localPoint._2, peak)) else None
    }
  }
}
val smallMapNavi = new Navigation(expectedMap, 4, 4)
smallMapNavi.newLoc(Point(1, 1, smallMapNavi.peaks(1)(1)))
import akka.agent.Agent

import scala.concurrent.{ExecutionContext, Future}
implicit val ec = ExecutionContext.fromExecutorService(new ForkJoinPool(6))
val finalAcc = Agent(List.empty[Int])
def skiing(start: Point, navi: Navigation, acc: Agent[List[Int]]) = {
  def _skiing(paths: List[Point], queue: List[Point]): Unit = {
    paths match {
      case Nil =>
      acc.send(x => maxRoute(navi.peaks(start.y)(start.x) :: queue.reverse.map(_.height), x))
      case z :: zs => {
        _skiing(navi.newLoc(Point(z.y, z.x, navi.peaks(z.y)(z.x))), z :: queue)
        if (zs.nonEmpty) _skiing(zs, queue)
      }
    }
  }

  _skiing(navi.newLoc(start), List.empty[Point])
}
def maxRoute(x: List[Int], y: List[Int]) = {
  def heightDiff(xs: List[Int]) = {
    xs.head - xs.last
  }
  if (x.length > y.length || (heightDiff(x) > heightDiff(y) && x.length == y.length)) x else y
}

def futureSkiing(start: Point, navi: Navigation, acc: Agent[List[Int]]) = Future {
    skiing(start, navi, acc)
}

def futureSkiing(navi: Navigation, acc: Agent[List[Int]]) = {
  def _futureSkiing(y: Int, x: Int): Unit = {
//    println(y + " : "  + x)
    Future {
      skiing(Point(y, x, navi.peaks(y)(x)), navi, acc)
    }
    if (x + 1 >= navi.maxX) _futureSkiing(y + 1, 0)
    else if (y < navi.maxY) _futureSkiing(y, x + 1)
  }

  _futureSkiing(0, 0)
}
val bigMap = Source.fromFile("/home/thlim/workspace/playground/coffee-break/src/main/resources/map.txt").getLines().drop(1).toList
val bigSlopes = bigMap.map(_.split(' ').toList.map(_.toInt))
val bigMapNavi = new Navigation(bigSlopes, bigSlopes.size, bigSlopes.size)
futureSkiing(bigMapNavi, finalAcc)
Thread.sleep(180000)
val x = finalAcc.get
println(x.size + " :: " + (x.head - x.last))
ec.shutdown()
