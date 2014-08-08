def flatMapSub[A,B](ls: Vector[A])(f: A => Vector[B]): Vector[B] = {
  @annotation.tailrec
  def helper(r: Vector[B], ls: Vector[A]): Vector[B] = {
    ls match {
      case Vector() => r
      case sublist@(head +: tail) => helper(r ++ f(head), tail)
    }
  }
  helper(Vector(), ls)
}

val v = (1 to 10).toVector

val f = (i : Int) => Vector(i + 1)

flatMapSub(v)(f)
