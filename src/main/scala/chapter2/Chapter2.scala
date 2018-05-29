package chapter2

import cats.instances.all._
import cats.kernel.Monoid
import cats.syntax.all._

object Chapter2 extends App {

  // testing boolean monoids
  println(BooleanMonoids(BooleanMonoids.and).combine(true, true)) // true
  println(BooleanMonoids(BooleanMonoids.or).combine(false, true)) // true
  println(BooleanMonoids(BooleanMonoids.xor).combine(true, true)) // false
  println(BooleanMonoids(BooleanMonoids.nxor).combine(false, true)) // false
  println(BooleanMonoids(BooleanMonoids.nxor).combine(true, true)) // true
  val order1 = Order(2.0, 2.0)
  val order2 = Order(1.0, 1.0)
  val orderList = List(order1, order2)

  println(add(List(1, 2, 3, 4)))
  println(addCats(List(1, 2, 3, 4)))
  println(addFold(List(1, 2, 3, 4)))

  // test superadder using simple fold
  def addFold(ints: List[Int]): Int = ints.fold(0)(_ + _)

  // test superadder using own monoid
  def add(ints: List[Int]): Int = {
    import IntMonoids._
    ints.fold(intAddMonoid.empty)((n1, n2) => intAddMonoid.combine(n1, n2))
  }

  println(addOptionCats(List(Some(1), None, Some(2), None)))
  println(addOptionCatSyntax(List(Some(1), None, Some(2), None)))

  // test superadder using cats' monoid
  def addCats(ints: List[Int]): Int = {
    ints.fold(Monoid[Int].empty)((n1, n2) => n1 |+| n2)
  }

  // test superadder for List[Option[Int]]
  def addOptionCats(ints: List[Option[Int]]): Option[Int] = {
    val moi = Monoid[Option[Int]]
    ints.fold(moi.empty)((o1, o2) => moi.combine(o1, o2))
  }

  println(addUnified(List(Some(1), None, Some(2), None)))
  println(addUnifiedContextBound(List(Some(1), None, Some(2), None)))

  def addOptionCatSyntax(ints: List[Option[Int]]): Option[Int] = {
    import cats.instances.all._
    import cats.syntax.all._
    val moi = Monoid[Option[Int]]
    ints.fold(moi.empty)((o1, o2) => o1 |+| o2)
  }

  implicit val orderMonoid: Monoid[Order] = new Monoid[Order] {

    override def empty = Order(0.0, 0.0)

    override def combine(x: Order, y: Order) = Order(x.totalCost |+| y.totalCost, x.quantity |+| y.quantity)
  }

  // test superadder unified as per book's solution
  def addUnified[A](ints: List[A])(implicit monoid: cats.Monoid[A]) = {
    ints.fold[A](monoid.empty)(_ |+| _)
  }

  def addUnifiedContextBound[A: cats.Monoid](ints: List[A]) = {
    ints.fold(cats.Monoid[A].empty)(_ |+| _)
  }

  // add order
  case class Order(totalCost: Double, quantity: Double)

  println(addUnified(orderList))
  println(addUnifiedContextBound(orderList))


}
