package chapter11

import cats.Monoid
import cats.instances.all._
import cats.syntax.all._

object GCounter {

  trait BoundedSemiLattice[A] extends Monoid[A] {
    def combine(a1: A, a2: A): A

    def empty: A
  }

  case class GCounterF(counters: Map[String, Int]) {

    def increment(machine: String, amount: Int) = {
      val value = amount + counters.getOrElse(machine, 0)
      GCounterF(counters + (machine -> value))
    }

    def merge(that: GCounterF): GCounterF = {
      val newCounters = that.counters ++ this.counters.map {
        case (k, v) => k -> (v max that.counters.getOrElse(k, 0))
      }
      GCounterF(newCounters)
    }

    def total: Int = counters.values.sum
  }

  case class GCounter[A](counters: Map[String, A]) {

    def increment(machine: String, item: A)(implicit m: Monoid[A]) = {
      val value = item |+| counters.getOrElse(machine, m.empty)
      GCounter(counters + (machine -> value))
    }

    def total(implicit m: Monoid[A]): A = counters.values.toList.combineAll

    def merge(that: GCounter[A])(implicit bsl: BoundedSemiLattice[A]) = {
      GCounter(counters |+| that.counters)
    }


  }

  object BoundedSemiLattice {
    implicit val intBSL: BoundedSemiLattice[Int] = new BoundedSemiLattice[Int] {
      override def combine(a1: Int, a2: Int) = a1 max a2

      override def empty = 0
    }

    implicit def setBSL[A](): BoundedSemiLattice[Set[A]] = new BoundedSemiLattice[Set[A]] {
      override def combine(a1: Set[A], a2: Set[A]) = a1 union a2

      override def empty = Set.empty[A]
    }
  }


}
