val smallMaze =
  """***** ***
    |*       *
    |* ****  *
    |*       *
    |****** **""".stripMargin

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

val rawMaze = smallMaze.split('\n').map(_.trim).toVector

case class Pos(y: Int, x: Int)

case class BluePrint(val rawMaze : Vector[String]) {
  val mappedPrint = rawMaze.zipWithIndex.map {
    case (s, row) => s.zipWithIndex.map {
      case (c, col) => c match {
        case '*' => None
        case _ => Some(Pos(row, col))
      }
    }
  }
  def isLegal(p : Pos) = mappedPrint.lift(p.y).flatMap(_.lift(p.x)).flatten match {
    case None => false
    case _ => true
  }
}

val bluePrint = BluePrint(rawMaze)

val mazeEntrance = bluePrint.mappedPrint.head.flatMap(x => x).head
val mazeExit = bluePrint.mappedPrint.last.flatMap(x => x).head
sealed abstract class Move() {
  def move(currentPos : Pos, bluePrint : BluePrint): Option[Pos]
  def isLegal(currentPos : Pos, bluePrint : BluePrint) = move(currentPos, bluePrint) match {
    case None => false
    case _ => true
  }
}

case class Up() extends Move {
  override def move(currentPos : Pos, bluePrint : BluePrint) = {
    val p = Pos(currentPos.y - 1, currentPos.x)
    if (bluePrint.isLegal(p)) Some(p) else None
  }
}

case class Right() extends Move {
  override def move(currentPos : Pos, bluePrint : BluePrint) = {
    val p = Pos(currentPos.y, currentPos.x + 1)
    if (bluePrint.isLegal(p)) Some(p) else None
  }
}

case class Down() extends Move {
  override def move(currentPos : Pos, bluePrint : BluePrint) = {
    val p = Pos(currentPos.y + 1, currentPos.x)
    if (bluePrint.isLegal(p)) Some(p) else None
  }
}

case class Left() extends Move {
  override def move(currentPos : Pos, bluePrint : BluePrint) = {
    val p = Pos(currentPos.y, currentPos.x - 1)
    if (bluePrint.isLegal(p)) Some(p) else None
  }
}

case class Marker(pos: Pos, history: Vector[Pos]) {
  def dirs(bluePrint : BluePrint) = Vector(Up(), Right(), Down(), Left()).filter(
    m => m.isLegal(pos, bluePrint) && !history.contains(m.move(pos, bluePrint).get))

  def move(bluePrint : BluePrint) : Vector[Marker] = dirs(bluePrint).map(m => Marker(m.move(pos, bluePrint).get, pos +: history))

  def hasArrivedAt(goal: Pos) = pos == goal
}

def paths(start: Pos, goal: Pos, bluePrint : BluePrint): Vector[Marker] = {
  @annotation.tailrec
  def _path(marker: Marker, markers : Vector[Marker], prevMarkers : Vector[Marker]): Vector[Marker] = {
    if (marker.hasArrivedAt(goal)) {
      println(marker)
      Marker(marker.pos, marker.pos +: marker.history) +: markers
    }
    else if (marker.dirs(bluePrint).isEmpty) {
      if (prevMarkers.isEmpty) Vector() else _path(prevMarkers.head, markers, prevMarkers.tail)
    }
    else {
      val newMarkers = marker.move(bluePrint)
      if (newMarkers.isEmpty) Vector() else _path(newMarkers.head, markers, newMarkers.tail ++ prevMarkers )
    }
  }

  Marker(start, Vector[Pos]()).dirs(bluePrint).flatMap(m => _path(Marker(m.move(start, bluePrint).get, start +: Vector()), Vector(), Vector()))
}

val allPaths = paths(mazeEntrance, mazeExit, bluePrint)
if (allPaths.isEmpty) {
  println("No solution found.")
} else {
  val shortestPath = allPaths.reduce((m,n) => if (m.history.size < n.history.size) m else n).history
  val solution = shortestPath.foldLeft(bluePrint.rawMaze)((raw, p) => raw.updated(p.y, raw(p.y).updated(p.x, '+')))
  solution.foreach(println)
}


