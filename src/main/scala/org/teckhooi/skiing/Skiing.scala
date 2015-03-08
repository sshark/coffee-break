package org.teckhooi.skiing

import scala.io.{BufferedSource, Codec, Source}
import scala.util.control.Exception._

/**
 *
 * @author Lim, Teck Hooi
 *
 *
 */

case class Point(y: Int, x: Int, height: Int)

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

object Skiing  extends App {

  type Routes = List[List[Point]]
  type Route = List[Point]

  def extractPeaksLayoutFrom(layoutInput: Source) = {
    val lines = layoutInput.getLines()
    val Array(width, height) = lines.next().split(" ").map(_.toInt)

    val peaksLayout = lines.toList.map(_.split(' ').map(_.toInt).toList)

    if (peaksLayout.size != height && peaksLayout(0).size != width) {
      Left(new IllegalArgumentException("Layout out actual size does not match definition"))
    } else Right(peaksLayout)
  }

  def slopesToWayPoints(l: List[List[Int]]) = l.zipWithIndex.flatMap(y =>
    y._1.zipWithIndex.flatMap(x =>
      List(Point(y._2, x._2, x._1))))

  def ski(start: Point, navi: Navigation): Routes = {
    def _ski(nextWayPoint: Route, route: Route, routeAccumulator: Routes): List[List[Point]] = {
      nextWayPoint match {
        case Nil => (start :: route.reverse) :: routeAccumulator
        case x :: xs => {
          val minorRouteAccumulator = _ski(navi.newLoc(x), x :: route, routeAccumulator)
          if (xs.isEmpty) minorRouteAccumulator else _ski(xs, route, minorRouteAccumulator)
        }
      }
    }

    _ski(navi.newLoc(start), List.empty[Point], List.empty[Route])
  }

  def longestWayPoints(routes: Routes) = {
    def _longestWayPoints(nextRoutes: Routes, result: Option[Route]): Option[Route] = {
      nextRoutes match {
        case Nil => result
        case x :: xs => result match {
          case None => _longestWayPoints(xs, Some(x))
          case Some(y) => if (x.length > y.length ||
            (wayPointHeightDiff(x) > wayPointHeightDiff(y) && x.length == y.length))
              _longestWayPoints(xs, Some(x))
          else _longestWayPoints(xs, Some(y))
        }
      }
    }

    def wayPointHeightDiff(xs: List[Point]) = {
      xs.head.height - xs.last.height
    }
    _longestWayPoints(routes, None)
  }

  def extractLongestRouteAndSteepestDrop(points: Option[List[Point]]) = {
    points match {
      case None => None
      case Some(l) => Some((l.foldLeft(List.empty[Int])((l, p) => p.height :: l).reverse,
        l.head.height - l.last.height))
    }
  }

  implicit val codec = Codec.UTF8

  val remote: Option[BufferedSource] = if (args.isEmpty) {
    catching(classOf[java.io.IOException]) opt Source.fromURL("http://s3-ap-southeast-1.amazonaws.com/geeks.redmart.com/coding-problems/map.txt")
  } else {
    Option(getClass.getResourceAsStream(args(0))) match {
      case Some(input) => catching(classOf[java.io.IOException]) opt Source.fromInputStream(input)
      case _ => None
    }
  }

  remote match {
    case None => println("Please provide the path to the peak layouts i.e. path (defined in classpath)\n" +
      "to a file. Alternatively, leave that option blank and the application will\n" +
      "download from predefined remote location. If you have left the option blank and\n" +
      "still see this message it means the file at the remote location is removed or moved.")
    case Some(input) => extractPeaksLayoutFrom(input) match {
      case Right(x) => {
        val wayPointsFound = longestWayPoints(slopesToWayPoints(x).flatMap(ski(_, new Navigation(x))))
        extractLongestRouteAndSteepestDrop(wayPointsFound) match {
          case None => println("No soiltion found.")
          case Some((longestRoute, steepest)) => println(s"Raw way points found: ${wayPointsFound.get}")
            println(s"Longest route = ${longestRoute.size}")
            println(s"Steepest = $steepest")
        }
      }
      case Left(x) => throw x
    }
  }
}
