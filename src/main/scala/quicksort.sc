val l = List(1, 4, 0, 2, 7, 3, -1, 2)
val s = List("aa", "Z", "bb")

def quicksort[A <% Ordered[A]](l: List[A]): List[A] = {
  l match {
    case Nil => List[A]()
    case x :: xs => quicksort(xs.filter(_ < x)) ++ List(x) ++ quicksort(xs.filter(_ >= x))
  }
}

quicksort[Int](l)

quicksort[String](s)
