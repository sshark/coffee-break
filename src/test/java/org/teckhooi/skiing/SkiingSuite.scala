package org.teckhooi.skiing

import org.junit.Assert._
import org.junit.Test
import org.teckhooi.skiing.Skiing._

import scala.io.{Codec, Source}
import scala.util.control.Exception._

/**
 *
 * @author Lim, Teck Hooi
 *
 *
 */

class SkiingSuite {
  val expectedLayout = List(List(4, 8, 7, 3),
    List(2, 5, 9, 3),
    List(6, 3, 2, 5),
    List(4, 4, 1, 6))

  implicit val codec = Codec.UTF8

  val source = Source.fromInputStream(getClass.getResourceAsStream("/small-map.txt"))

  @Test
  def testReadingLayoutAsResource(): Unit = {
    extractPeaksLayoutFrom(source) match {
      case Right(layout) => assertTrue(layout == expectedLayout)
      case Left(ex) => throw ex
    }
  }

  @Test
  def slopesToPoint(): Unit = {
    extractPeaksLayoutFrom(source) match {
      case Right(layout) => assertEquals(16, slopesToWayPoints(layout).size)
      case Left(ex) => throw ex
    }
  }

  @Test
  def testSkiWithNavigation(): Unit = {
    extractPeaksLayoutFrom(source) match {
      case Right(layout) => val wayPoints = slopesToWayPoints(layout)
        assertEquals(16, wayPoints.size)
        assertTrue(List(List(Point(0,0,4), Point(1,0,2))) == ski(wayPoints(0), new Navigation(layout)))
      case Left(ex) => throw ex
    }
  }

  @Test
  def testLongestRouteFound(): Unit = {
    extractPeaksLayoutFrom(source) match {
      case Right(layout) => val wayPoints = slopesToWayPoints(layout)
        assertEquals(16, wayPoints.size)
        assertTrue(List(Point(1,2,9), Point(1,1,5), Point(2,1,3), Point(2,2,2), Point(3,2,1)) ==
          longestWayPoints(wayPoints.flatMap(ski(_, new Navigation(layout)))).get)
      case Left(ex) => throw ex
    }
  }

  @Test
  def testReadFromHTTP(): Unit = {
    val remote = catching(classOf[java.io.IOException]) opt Source.fromURL("http://s3-ap-southeast-1.amazonaws.com/geeks.redmart.com/coding-problems/map.txt") match {
      case None => catching(classOf[java.io.IOException]) either Source.fromInputStream(getClass.getResourceAsStream("/small-map.txt")) match {
        case Right(x) => x
        case Left(x) => throw x
      }
      case Some(x) => x
    }

    extractPeaksLayoutFrom(remote) match {
      case Right(layout) => val wayPoints = slopesToWayPoints(layout)
        assertEquals(1000000, wayPoints.size)
      case Left(ex) => throw ex
    }

    val local = catching(classOf[java.io.IOException]) opt Source.fromURL("http://s3-ap-southeast-1.amazonaws.com/geeks.redmart.com/coding-problems/map-not-found.txt") match {
      case None => catching(classOf[java.io.IOException]) either Source.fromInputStream(getClass.getResourceAsStream("/small-map.txt")) match {
        case Right(x) => x
        case Left(x) => throw x
      }
      case Some(x) => x
    }

    extractPeaksLayoutFrom(local) match {
      case Right(layout) => val wayPoints = slopesToWayPoints(layout)
        assertEquals(16, wayPoints.size)
      case Left(ex) => throw ex
    }
  }
}
