package chapter2

trait Semigroup[A] {
  def combine(a1: A, a2: A): A
}

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

object BooleanMonoids {
  def apply[A](implicit monoid: Monoid[A]) = monoid

  implicit val and: Monoid[Boolean] =
    new Monoid[Boolean] {
      override def combine(a1: Boolean, a2: Boolean) = a1 && a2

      override def empty = true
    }

  implicit val or: Monoid[Boolean] =
    new Monoid[Boolean] {
      override def combine(a1: Boolean, a2: Boolean) = a1 || a2

      override def empty = false
    }

  implicit val xor: Monoid[Boolean] =
    new Monoid[Boolean] {
      override def combine(a1: Boolean, a2: Boolean) = !(a1 && a2) && (a1 || a2)

      override def empty = false
    }

  implicit val nxor: Monoid[Boolean] =
    new Monoid[Boolean] {
      override def combine(a1: Boolean, a2: Boolean) = !(a1 || a2) || (a1 && a2)

      override def empty = true
    }

}

