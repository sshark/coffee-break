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
case class Point(y: Int, x: Int, height: Int)
def slopesToPoints(l: List[List[Int]]) = l.zipWithIndex.flatMap (y =>
  y._1.zipWithIndex.flatMap (x =>
    List(Point(y._2, x._2, x._1))))
val points = slopesToPoints(slopes)
points.size
class Navigation(val slopes: List[List[Int]]) {
  val movements: List[Tuple2[Int, Int]] = List((1, 0), (0, 1), (-1, 0), (0, -1))

  def newLoc(loc: Point) = {
    val moves = movements.map(d => Point(d._1 + loc.y, d._2 + loc.x, loc.height))

    moves.flatMap(p => slopes.lift(p.y).flatMap(_.lift(p.x) match {
      case Some(x) => if (x < p.height) Some(Point(p.y, p.x, x)) else None
      case _ => None
    }))
  }
}
val smallMapNavi = new Navigation(expectedMap)
smallMapNavi.newLoc(Point(1, 1, 4))

import akka.agent.Agent

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

val executorService = Executors.newFixedThreadPool(16)

implicit val ec = ExecutionContext.fromExecutorService(executorService)

val finalAcc = Agent(List.empty[Point])

def skiing(start: Point, navi: Navigation, acc: Agent[List[Point]]) = {
  def _skiing(paths: List[Point], queue: List[Point]): Unit = {
    paths match {
      case Nil => acc.send(x => maxRoute(start :: queue.reverse, x))
      case x :: xs => {
        _skiing(navi.newLoc(x), x :: queue)
        if (xs.nonEmpty) _skiing(xs, queue)
      }
    }
  }

  _skiing(navi.newLoc(start), List.empty[Point])
}

def maxRoute(x: List[Point], y: List[Point]) = {
  def heightDiff(xs: List[Point]) = {
    xs.head.height - xs.last.height
  }
  if (x.length > y.length || (heightDiff(x) > heightDiff(y) && x.length == y.length)) x else y
}

def futureSkiing(start: Point, navi: Navigation, acc: Agent[List[Point]]) = Future {
    skiing(start, navi, acc)
}
val bigMap = Source.fromFile("/home/thlim/workspace/playground/coffee-break/src/main/resources/map.txt").getLines().drop(1).toList
val bigSlopes = bigMap.map(_.split(' ').toList.map(_.toInt))
val bigMapNavi = new Navigation(bigSlopes)
val bigPoints = slopesToPoints(bigSlopes)
val futureRoutes = Future.sequence(bigPoints.map(futureSkiing(_, bigMapNavi, finalAcc)))
Await.result(futureRoutes, 5 minute)
val x = finalAcc.get()
println(x.size + " :: " + (x.head.height - x.last.height))
ec.shutdown()
