def combi[A](l : List[A]) : List[List[A]] = {
  def _combi(k : List[A], ll : List[List[A]]) : List[List[A]] = {
    if (k.isEmpty) ll
    else _combi(k.tail, __combi(k.head, ll, ll))
  }

  def __combi(a : A, la : List[List[A]], result : List[List[A]]) : List[List[A]] = {
    if (la.isEmpty) result
    else __combi(a, la.tail, (a :: la.head) ::  result)
  }

  _combi(l, List(List.empty[A]))
}

combi(List(1,2,3,3,4,5))


