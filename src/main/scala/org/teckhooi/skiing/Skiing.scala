package org.teckhooi.skiing

import scala.io.{BufferedSource, Source}
import scala.util.control.Exception._

/**
 *
 * @author Lim, Teck Hooi
 *
 *
 */

case class Point(y: Int, x: Int, h: Int)

class Navigation(val peaks: List[List[Int]]) {
  val maxY = peaks.size
  val maxX = peaks(0).size
  val movements = List((1, 0), (0, 1), (-1, 0), (0, -1))
  def newLoc(y: Int, x: Int, height: Int) = {
    val moves = movements.map(d => (d._1 + y, d._2 + x)).filter(d => d._1 >= 0 && d._1 < maxY && d._2 >= 0 && d._2 < maxX)
    moves.flatMap{case p =>
      val peak = peaks(p._1 % maxY)(p._2 % maxX)
      if (peak < height) Some(Point(p._1, p._2, peak)) else None
    }
  }
}

object Skiing  extends App {

  type Routes = List[Route]
  type Route = List[Point]

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

  def extractPeaksLayoutFrom(layoutInput: Source) = {
    val lines = layoutInput.getLines()
    val Array(width, height) = lines.next().split(" ").map(_.toInt)

    val peaksLayout = lines.toList.map(_.split(' ').map(_.toInt).toList)

    if (peaksLayout.size != height && peaksLayout(0).size != width) {
      Left(new IllegalArgumentException("Layout out actual size does not match definition"))
    } else Right(peaksLayout)
  }

  val remote: Option[BufferedSource] = args.headOption flatMap {filename =>
    Option(getClass.getResourceAsStream(filename)) map {input =>
      catching(classOf[java.io.IOException]).apply(Source.fromInputStream(input))
    }
  }

  remote match {
    case None => println("Please provide the path to the peak layouts i.e. path (defined in classpath)\n" +
      "to a file. Alternatively, leave that option blank and the application will\n" +
      "download from predefined remote location. If you have left the option blank and\n" +
      "still see this message it means the file at the remote location is removed or moved.")
    case Some(input) => extractPeaksLayoutFrom(input) match {
      case Right(x) => {
        val result = futureSkiing(new Navigation(x))
        println(s"${result.size} :: ${result.head - result.last}")
      }
      case Left(x) => throw x
    }
  }
}
