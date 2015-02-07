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

def skiing(start: Point, navi: Navigation): List[List[Point]] = {
  def _skiing(paths: List[Point], queue: List[Point], acc: List[List[Point]]): List[List[Point]] = {
    paths match {
      case Nil => (start :: queue.reverse) :: acc
      case x :: xs => {
        val localAcc = _skiing(navi.newLoc(x), x :: queue, acc)
        if (xs.isEmpty) localAcc else _skiing(xs, queue, localAcc)
      }
    }
  }

  _skiing(navi.newLoc(start), List.empty[Point], List.empty[List[Point]])
}

skiing(Point(1, 1, 4), smallMapNavi)
skiing(Point(0, 1, 8), smallMapNavi)

def longestRoute(routes: List[List[Point]]) = {
  def _longestRoutes(nextRoutes: List[List[Point]], acc: Option[List[Point]]): Option[List[Point]] = {
    nextRoutes match {
      case Nil => acc
      case x :: xs => acc match {
        case None => _longestRoutes(xs, Some(x))
        case Some(y) => if (x.length > y.length || (heightDiff(x) > heightDiff(y) && x.length == y.length))
          _longestRoutes(xs, Some(x))
          else _longestRoutes(xs, Some(y))
      }
    }
  }

  def heightDiff(xs: List[Point]) = {
    xs.head.height - xs.last.height
  }
  _longestRoutes(routes, None)
}

def extractLongestRouteAndSteepest(points: Option[List[Point]]) = {
  points match {
    case None => None
    case Some(l) => Some((l.foldLeft(List.empty[Int])((l, p) => p.height :: l).reverse,
      l.head.height - l.last.height))
  }
}
val path = longestRoute(points.flatMap(skiing(_, smallMapNavi)))
val smallResult = extractLongestRouteAndSteepest(path)
println(s"Route = ${path.get}")
println(s"Longest route = ${smallResult.get._1.size}")
println(s"Steepest = ${smallResult.get._2}")
val bigMap = Source.fromFile("/home/thlim/workspace/playground/coffee-break/src/main/resources/map.txt").getLines().drop(1).toList
val bigSlopes = bigMap.map(_.split(' ').toList.map(_.toInt))
val bigMapNavi = new Navigation(bigSlopes)
bigSlopes.size
bigSlopes(0).size
bigSlopes(1)(2)
bigSlopes(999)(999)
val bigPoints = slopesToPoints(bigSlopes)
val bigPath = longestRoute(bigPoints.flatMap(skiing(_, bigMapNavi)))
val bigResult = extractLongestRouteAndSteepest(bigPath)
println(s"Route = ${bigPath.get}")
println(s"Longest route = ${bigResult.get._1.size}")
println(s"Steepest = ${bigResult.get._2}")
