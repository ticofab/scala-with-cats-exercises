package chapter2

trait Semigroup[A] {
  def combine(a1: A, a2: A): A
}

trait MyMonoid[A] extends Semigroup[A] {
  def empty: A
}

object BooleanMonoids {
  def apply[A](implicit monoid: MyMonoid[A]) = monoid

  implicit val and: MyMonoid[Boolean] =
    new MyMonoid[Boolean] {
      override def combine(a1: Boolean, a2: Boolean) = a1 && a2

      override def empty = true
    }

  implicit val or: MyMonoid[Boolean] =
    new MyMonoid[Boolean] {
      override def combine(a1: Boolean, a2: Boolean) = a1 || a2

      override def empty = false
    }

  implicit val xor: MyMonoid[Boolean] =
    new MyMonoid[Boolean] {
      override def combine(a1: Boolean, a2: Boolean) = !(a1 && a2) && (a1 || a2)

      override def empty = false
    }

  implicit val nxor: MyMonoid[Boolean] =
    new MyMonoid[Boolean] {
      override def combine(a1: Boolean, a2: Boolean) = !(a1 || a2) || (a1 && a2)

      override def empty = true
    }

}
