case class Node[A](value: A, children: Seq[Node[A]] = Nil, parent: Option[Node[A]] = None) {
  def toListFromRoot: Seq[A] = {
    parent match {
      case None => Seq(value)
      case Some(next) => next.toListFromRoot ++ Seq(value)
    }
  }
}
val node = Node('a,
  Seq(
    Node('b,
      Seq(Node('e), Node('f), Node('g))),
    Node('c,
      Seq(Node('h), Node('i, Seq(Node('k))))),
    Node('d,
      Seq(Node('j)))))
def paths[A](v: Node[A]) = {
  def _paths(current: Node[A], peers: Seq[Node[A]], result: Vector[Vector[A]]): Vector[Vector[A]] = {
    if (peers.isEmpty) result
    else {
      val nextNodes = peers.foldLeft(Vector.empty[Vector[Node[A]]])((vv, v) => v.children.toVector +: vv)
      val nextValues = nextNodes.filterNot(_.isEmpty).map(_.map(_.value)).toVector
      _paths(peers.head, nextNodes.flatten, nextValues ++ result)
    }
  }
  _paths(v, Seq(v), Vector(Vector(v.value))).reverse
}
// expects
// Vector(Vector('a), Vector('b, 'c, 'd), Vector('e, 'f, 'g), Vector('h, 'i), Vector('j))
val pathsMapped = paths(node)
if (pathsMapped == Vector(Vector('a), Vector('b, 'c, 'd), Vector('e, 'f, 'g), Vector('h, 'i), Vector('j))) {
  println("Matched: " + pathsMapped)
} else {
  println("Not matched.")
}
def bfsPaths[A](v: Node[A], end: Node[A]) = {
  def _bfsPaths(current: Node[A], queue: Seq[Node[A]]): Option[Node[A]] = {
    if (end.value == current.value) {
      Some(current)
    } else {
      val childrenNode = current.children.map(node => node.copy(parent = Some(current)))
      val newQueue = queue ++ childrenNode
      if (newQueue.isEmpty) None
      else _bfsPaths(newQueue.head, newQueue.tail)
    }
  }

  _bfsPaths(v, Nil)
}

// expects
// 'a -> 'c -> 'i -> 'k
bfsPaths(node, Node('k)).map(_.toListFromRoot.mkString(" -> ")).getOrElse("No solution.")
