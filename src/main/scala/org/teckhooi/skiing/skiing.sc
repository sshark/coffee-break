import scala.io.Source
val expectedMap = List(List(4, 8, 7, 3),
  List(2, 5, 9, 3),
  List(6, 3, 2, 5),
  List(4, 4, 1, 6))
val userHome = System.getProperty("user.home")
val textSlopes = Source.fromFile(s"$userHome/workspace/playground/coffee-break/src/main/resources/small-map.txt").getLines().drop(1).toList
val slopes = textSlopes.map(_.split(' ').map(_.toInt).toList)
slopes.size
case class Point(y: Int, x: Int, h: Int)
class Navigation(val peaks: List[List[Int]], val maxY: Int, val maxX: Int) {
  val movements = List((1, 0), (0, 1), (-1, 0), (0, -1))
  def newLoc(y: Int, x: Int, height: Int) = {
    val moves = movements.map(d => (d._1 + y, d._2 + x)).filter(d => d._1 >= 0 && d._1 < maxY && d._2 >= 0 && d._2 < maxX)
    moves.flatMap{case p =>
      val peak = peaks(p._1 % maxY)(p._2 % maxX)
      if (peak < height) Some(Point(p._1, p._2, peak)) else None
    }
  }
}
val smallMapNavi = new Navigation(expectedMap, 4, 4)
smallMapNavi.newLoc(1, 1, smallMapNavi.peaks(1)(1))
def skiing(y: Int, x: Int, navi: Navigation): List[Int] = {
  def _skiing(paths: List[Point], queue: List[Point], acc: List[Int]): List[Int] = {
    paths match {
      case Nil => maxRoute(navi.peaks(y)(x) :: queue.reverse.map(_.h), acc)
      case z :: zs => {
        val localAcc = _skiing(navi.newLoc(z.y, z.x, navi.peaks(z.y)(z.x)), z :: queue, acc)
        if (zs.isEmpty) localAcc else _skiing(zs, queue, localAcc)
      }
    }
  }

  _skiing(navi.newLoc(y, x, navi.peaks(y)(x)), List.empty[Point], List.empty[Int])
}
skiing(1, 1, smallMapNavi)
skiing(0, 1, smallMapNavi)
def futureSkiing(navi: Navigation) = {
  // used Iterator instead of List before grouping. If List is used, make sure the Scala is version 2.11.6
  // otherwise the number of elements returned from the parallel collection will not be the same as before
  (0 until (navi.maxY * navi.maxX)).grouped(240).foldLeft(List.empty[Int]){case (l, tuple) =>
    val batches = tuple.par.map{x =>
      skiing(x / navi.maxY, x % navi.maxX, navi)
    }
    maxRoute(l, batches.reduce(maxRoute(_, _)))
  }
}
def maxRoute(x: List[Int], y: List[Int]) = {
  def heightDiff(xs: List[Int]) = {
    xs.head - xs.last
  }
  if (x.isEmpty) y
  else if (y.isEmpty) x
  else if (x.length > y.length || (heightDiff(x) > heightDiff(y) && x.length == y.length)) x
  else y
}
val bigMap = Source.fromFile(s"$userHome/workspace/playground/coffee-break/src/main/resources/map.txt").getLines().drop(1).toList
val bigSlopes = bigMap.map(_.split(' ').toList.map(_.toInt))
val bigMapNavi = new Navigation(bigSlopes,  bigSlopes.size,  bigSlopes.size)
val result = futureSkiing(bigMapNavi)
println(s"${result.size} :: ${result.head - result.last}")
