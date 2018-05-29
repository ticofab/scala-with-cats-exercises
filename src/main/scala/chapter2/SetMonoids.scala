package chapter2

object SetSemigroups {
  def setIntersectionMonoid[A]: Semigroup[Set[A]] = {
    (a1: Set[A], a2: Set[A]) => a1 intersect a2
  }
}

object SetMonoids {
  def unionMonoid[A]: MyMonoid[Set[A]] = {
    new MyMonoid[Set[A]] {
      override def combine(a1: Set[A], a2: Set[A]) = a1 union a2

      override def empty = Set.empty[A]
    }
  }
}
