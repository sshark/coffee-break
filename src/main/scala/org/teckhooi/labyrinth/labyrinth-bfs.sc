val smallMaze =
  """***** ***
    |*       *
    |*   *****
    |*       *
    |****** **""".stripMargin

val crazyMaze =
  """************************* *************************
    |* * * * * * * * * * * * * * * * * * * * * * * * * *
    |*  *   *   *   *   *   *   *   *   *   *   *   *  *
    |* *  *   *   *   *   *   *   *   *   *   *   *  * *
    |*  *   *   *   *   *   *   *   *   *   *   *   *  *
    |* *  *   *   *   *   *   *   *   *   *   *   *  * *
    |*  *   *   *   *   *   *   *   *   *   *   *   *  *
    |* *  *   *   *   *   *   *   *   *   *   *   *  * *
    |*  *   *   *   *   *   *   *   *   *   *   *   *  *
    |* *  *   *   *   *   *   *   *   *   *   *   *  * *
    |*  *   *   *   *   *   *   *   *   *   *   *   *  *
    |* *  *   *   *   *   *   *   *   *   *   *   *  * *
    |*  *   *   *   *   *   *   *   *   *   *   *   *  *
    |* *  *   *   *   *   *   *   *   *   *   *   *  * *
    |*  *   *   *   *   *   *   *   *   *   *   *   *  *
    |* *  *   *   *   *   *   *   *   *   *   *   *  * *
    |*  *   *   *   *   *   *   *   *   *   *   *   *  *
    |* *  *   *   *   *   *   *   *   *   *   *   *  * *
    |*  *   *   *   *   *   *   *   *   *   *   *   *  *
    |* *  *   *   *   *   *   *   *   *   *   *   *  * *
    |*  *   *   *   *   *   *   *   *   *   *   *   *  *
    |*  *   *   *   *   *   *   *   *   *   *   *   *  *
    |* *  *   *   *   *   *   *   *   *   *   *   *  * *
    |*  *   *   *   *   *   *   *   *   *   *   *   *  *
    |* *  *   *   *   *   *   *   *   *   *   *   *  * *
    |*  *   *   *   *   *   *   *   *   *   *   *   *  *
    |* *  *   *   *   *   *   *   *   *   *   *   *  * *
    |*  *   *   *   *   *   *   *   *   *   *   *   *  *
    |* *  *   *   *   *   *       *   *   *   *   *  * *
    |************************* *************************""".stripMargin
val mediumMaze =
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
val rawMaze = mediumMaze.split('\n').map(_.trim).toVector
case class Pos(y: Int, x: Int)
case class BluePrint(rawMaze : Vector[String]) {
  val mappedPrint = rawMaze.zipWithIndex.map {
    case (s, row) => s.zipWithIndex.map {
      case (c, col) => c match {
        case '*' => None
        case _ => Some(Pos(row, col))
      }
    }
  }

  def isLegal(p: Pos) = if (p.y >= 0 && p.y < mappedPrint.size && p.x >= 0 && p.x < mappedPrint(0).size)
    mappedPrint(p.y)(p.x) match {
      case None => false
      case _ => true
    } else false
}

val bluePrint = BluePrint(rawMaze)

val mazeEntrance = bluePrint.mappedPrint.head.flatMap(x => x).head
val mazeExit = bluePrint.mappedPrint.last.flatMap(x => x).head
sealed abstract class Move() {
  def move(currentMarker : Marker, bluePrint : BluePrint): Option[Marker] = {
    val newPos = nextPos(currentMarker, bluePrint)
    if (bluePrint.isLegal(newPos))
      Some(currentMarker.copy(pos = newPos, parent = Some(currentMarker))) else None
  }

  def nextPos(currentMarker : Marker, bluePrint : BluePrint): Pos
}
case class Up() extends Move {
  override def nextPos(currentMarker : Marker, bluePrint : BluePrint) = {
    Pos(currentMarker.pos.y - 1, currentMarker.pos.x)
  }
}

case class Right() extends Move {
  override def nextPos(currentMarker : Marker, bluePrint : BluePrint) = {
    Pos(currentMarker.pos.y, currentMarker.pos.x + 1)
  }
}
case class Down() extends Move {
  override def nextPos(currentMarker : Marker, bluePrint : BluePrint) = {
    Pos(currentMarker.pos.y + 1, currentMarker.pos.x)
  }
}
case class Left() extends Move {
  override def nextPos(currentMarker : Marker, bluePrint : BluePrint) = {
    Pos(currentMarker.pos.y, currentMarker.pos.x - 1)
  }
}

case class Marker(pos: Pos, parent: Option[Marker]) {
  def neighbours(bluePrint : BluePrint) = Seq(Up(), Right(), Down(), Left()).flatMap(m => m.move(this, bluePrint))

  def hasArrivedAt(goal: Pos) = pos == goal
  def toListFromRoot: Seq[Pos] = {
    parent match {
      case None => Seq(pos)
      case Some(next) => next.toListFromRoot ++ Seq(pos)
    }
  }
}
def bfsPaths(start: Pos, goal: Pos, bluePrint : BluePrint): Option[Marker] = {
  @annotation.tailrec
  def _bfsPaths(marker: Marker, queue: Seq[Marker], history: Seq[Pos]): Option[Marker] = {
    if (marker.hasArrivedAt(goal)) Some(marker)
    else {
      val newNeighbours = marker.neighbours(bluePrint)
      val newQueue = (queue ++ newNeighbours).filterNot(m => history.contains(m.pos))
      if (newQueue.isEmpty) None
      else _bfsPaths(newQueue.head, newQueue.tail, newQueue.head.pos +: history)
    }
  }

  _bfsPaths(Marker(start, None), Nil, Nil)
}
val path = bfsPaths(mazeEntrance, mazeExit, bluePrint).map(_.toListFromRoot)
path match {
  case Some(node) => {
    val solution = node.foldLeft(bluePrint.rawMaze)((raw, p) => raw.updated(p.y, raw(p.y).updated(p.x, '+')))
    solution.foreach(println)
  }
  case None => println("No solution.")
}

