val twoWaysMaze =
  """***** ***
    |*       *
    |* ***** *
    |*       *
    |****** **""".stripMargin

val twoWaysRaw = twoWaysMaze.split('\n').map(_.trim).toVector

val amazingMaze =
  """************************* *************************
    |*                                   * *           *
    |* * *** *** ******************* ***** * * * * * ***
    |* * *   * *   *   * * *                 * * * *   *
    |***** * * *** * *** * * *** *** * ***** *** *******
    |*     * *   * *     *   * * *   *     * * *       *
    |*** ******* * ***** *** * * ******* * *** * *** * *
    |* *     *     *   *     *     *     * *       * * *
    |* * *********** * ********* * ******* * *** * *****
    |*     * *   * * *     *     * *   *   * *   *     *
    |* ***** * *** * ***** *** *** * * * ******* ***** *
    |* *     *   * * *       * * *   * * * *   *     * *
    |* * ***** *** *** *** *** * ***** *** *** ***** ***
    |*     *   * * *     * *       * *       * *     * *
    |* * ***** * * * *** * *** ***** *** ***** *** * * *
    |* * *           *   * *   *     *     *     * *   *
    |* ******* ******* * *** ******* *** * * ********* *
    |*   *       *     * *   *         * * * *     *   *
    |*** * * ***** * ***** ******* ******* * * * * * ***
    |*     *   *   *         *       * *   * * * * *   *
    |*** * *** * *** ***** ******* * * * *** *** * *** *
    |* * * * * * * *     * * *     *       *   * * * * *
    |* * *** * * * *** *** * * ********* ***** * * * * *
    |* * *   * * *     *   * *   *     *   *     * * * *
    |* * * *** ******* ***** * ******* *** * *** *** * *
    |* * *     *   *   *     * *     * * * *   *   * * *
    |* ***** * * * *** * ***** ***** * * * ***** * * * *
    |* *     * * * *     * *     *           * * *   * *
    |* ***** * *** * ***** *********** ******* * * * * *
    |*     * * * *             *   *     * * *   * * * *
    |* * * *** * *** * ***** ***** ******* * *** * * * *
    |* * *   * * *   *     * *             *     * * * *
    |* ***** * * *********** ******* *** * ******* * * *
    |* *     *   *   *     * *   *   * * *       * *   *
    |* * * ********* * ***** * *** *** *** * ***** * ***
    |* * *       *           *   * * *   * *   *   *   *
    |* ******* ***** ******* * *** * * *** *** * *******
    |*   *   *   *   *   *     *         * * * * * * * *
    |* ***** * *** ***** * ******* * ***** * *** * * * *
    |*     *           *     *     * * *   *   *     * *
    |*** *** ********************* *** *** *** *** * * *
    |*   *   *     *               * * *   *       *   *
    |*** *** * ***** * ******* *** * * *** * *** ***** *
    |*       *       *   *   * * *   *     *   * *   * *
    |*** ***** ***** *** *** *** ***** * * *** *** * * *
    |*       *   *   * * *       *   * * *   * *   *   *
    |*** *** * ***** * ***** *** *** *** *** ******* ***
    |*   *     *   *   *     * * * *     * * *     *   *
    |* ***** *** ***** ******* * * *** *** * *** ***** *
    |*   *                 *           *         *     *
    |************************* *************************""".stripMargin

case class Pos(y: Int, x: Int)

val amazingRaw = amazingMaze.split('\n').map(_.trim).toVector

case class BluePrint(val rawPrint : Vector[String]) {
  val mappedPrint = rawPrint.zipWithIndex.map {
    case (s, row) => s.zipWithIndex.map {
      case (c, col) => c match {
        case '*' => None
        case _ => Some(Pos(row, col))
      }
    }
  }
  def isLegal(p : Pos) = false
}
val bluePrint = BluePrint(amazingRaw)

val mazeEntrance = bluePrint.mappedPrint.head.flatMap(x => x).head
val mazeExit = bluePrint.mappedPrint.last.flatMap(x => x).head
sealed abstract class Move {
  def move(p: Pos): Pos

  def isLegal(p: Pos): Boolean

  def isLegal(f: Option[Pos]) = {
    f match {
      case None => false
      case _ => true
    }
  }
}

case class Up() extends Move {
  override def move(p: Pos) = {
    Pos(p.y - 1, p.x)
  }

  override def isLegal(p: Pos) = {
    isLegal(bluePrint.mappedPrint.lift(p.y - 1).flatMap(_.lift(p.x)).flatten)
  }
}

case class Right() extends Move {
  override def move(p: Pos) = {
    Pos(p.y, p.x + 1)
  }

  override def isLegal(p: Pos) = {
    isLegal(bluePrint.mappedPrint.lift(p.y).flatMap(_.lift(p.x + 1)).flatten)
  }
}

case class Down() extends Move {
  override def move(p: Pos) = {
    Pos(p.y + 1, p.x)
  }

  override def isLegal(p: Pos) = {
    isLegal(bluePrint.mappedPrint.lift(p.y + 1).flatMap(_.lift(p.x)).flatten)
  }
}

case class Left() extends Move {
  override def move(p: Pos) = {
    Pos(p.y, p.x - 1)
  }

  override def isLegal(p: Pos) = {
    isLegal(bluePrint.mappedPrint.lift(p.y).flatMap(_.lift(p.x - 1)).flatten)
  }
}

case class Marker(pos: Pos, history: Vector[Pos]) {
  val moves = Vector(Up(), Right(), Down(), Left()).filter(m => m.isLegal(pos) && !history.contains(pos))

  def move: Vector[Marker] = moves.map(m => Marker(m.move(pos), pos +: history))

  def hasArrivedAt(goal: Pos) = pos == goal
}

def paths(start: Pos, goal: Pos): Vector[Marker] = {
  def _paths(marker: Marker, markers : Vector[Marker]): Vector[Marker] = {
    if (marker.hasArrivedAt(goal)) Marker(marker.pos, marker.pos +: marker.history) +: markers
    else if (marker.moves.isEmpty) markers
    else {
      marker.move.flatMap(m => _paths(m, markers))
    }
  }
  Marker(start, Vector[Pos]()).moves.flatMap(m => _paths(Marker(m.move(start), start +: Vector()), Vector()))
}

val shortestPath = paths(mazeEntrance, mazeExit).reduce((m,n) => if (m.history.size < n.history.size) m else n).history

val solution = shortestPath.foldLeft(bluePrint.rawPrint)((raw, p) => raw.updated(p.y, raw(p.y).updated(p.x, '+')))

solution.foreach(println)



