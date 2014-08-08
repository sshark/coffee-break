val l = List(2,5,1,6,2,0)

val s = List("pear", "apple", "banana")

def mergeSort[A <% Ordered[A]](l : List[A]) : List[A] = {
  def _merge(l : List[A], r : List[A]) : List[A] = {
    (l, r)  match {
      case (x, Nil) => x
      case (Nil, y) => y
      case (x :: xs, y :: ys) =>
        if (x > y) {
          y :: _merge(l, ys)
        } else {
          x :: _merge(xs, r)
        }
    }
  }

  val ndx = l.size / 2
  if (ndx == 0) {
    l
  } else {
    val (lh, rh) = l splitAt ndx
    _merge(mergeSort(lh), mergeSort(rh))
  }
}

def quicksort[A <% Ordered[A]](l: List[A]): List[A] = {
  l match {
    case Nil => List[A]()
    case x :: xs => quicksort(xs.filter(_ < x)) ++ List(x) ++ quicksort(xs.filter(_ >= x))
  }
}

mergeSort(l)
quicksort(l)
mergeSort(s)
quicksort(s)
