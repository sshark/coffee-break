package org.teckhooi.skiing

import org.junit.Assert._
import org.junit.Test
import org.teckhooi.skiing.Skiing._

import scala.io.Source

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

  val source = Source.fromInputStream(getClass.getResourceAsStream("/small-map.txt"))

  @Test
  def testReadingLayoutAsResource(): Unit = {
    extractPeaksLayoutFrom(source) match {
      case Right(layout) => assertTrue(layout == expectedLayout)
      case Left(ex) => throw ex
    }
  }

  @Test
  def testSkiingWithNavigation(): Unit = {
    extractPeaksLayoutFrom(source) match {
      case Right(layout) =>
        assertEquals(4, layout.size)
        assertEquals(4, layout(0).size)
        assertEquals(List(4, 2), skiing(0, 0, new Navigation(layout)))
      case Left(ex) => throw ex
    }
  }

  @Test
  def testFindLongestSkiingRouteWithSingleThread(): Unit = {
    extractPeaksLayoutFrom(source) match {
      case Right(layout) =>
        val navi = new Navigation(layout)
        assertEquals(List(9, 5, 3, 2, 1),
          (0 until (navi.maxY * navi.maxX)).map(x => skiing(x / navi.maxY, x % navi.maxX, navi)).reduce(maxRoute))
      case Left(ex) => throw ex
    }
  }

  @Test
  def testFindLongestSkiingRouteParallel(): Unit = {
    extractPeaksLayoutFrom(source) match {
      case Right(layout) =>
        val navi = new Navigation(layout)
        assertEquals(List(9, 5, 3, 2, 1),
          (0 until (navi.maxY * navi.maxX)).grouped(2).foldLeft(List.empty[Int]){case (l, tuple) =>
            val batches = tuple.par.map{x =>
              skiing(x / navi.maxY, x % navi.maxX, navi)
            }
            maxRoute(l, batches.reduce(maxRoute(_, _)))
          })
      case Left(ex) => throw ex
    }
  }
}
