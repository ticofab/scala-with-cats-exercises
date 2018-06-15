package chapter11

import cats._
import cats.instances.all._
import cats.syntax.all._
import chapter11.GCounter1.BoundedSemiLattice

trait GCounter2[F[_, _], K, V] {
  def increment(f: F[K, V])(k: K, v: V)
               (implicit m: Monoid[V]): F[K, V]

  def merge(f1: F[K, V], f2: F[K, V])
           (implicit b: BoundedSemiLattice[V]): F[K, V]

  def total(f: F[K, V])
           (implicit m: Monoid[V]): V
}

object GCounter2 {
  def apply[F[_, _], K, V](implicit counter: GCounter2[F, K, V]) = counter

  implicit def mapGCounter2[K, V] = new GCounter2[Map, K, V] {

    override def increment(f: Map[K, V])(k: K, v: V)(implicit m: Monoid[V]) = {
      val value = v |+| f.getOrElse(k, m.empty)
      f + (k -> value)
    }

    override def merge(f1: Map[K, V], f2: Map[K, V])(implicit b: BoundedSemiLattice[V]) = {
      // the way I came up with myself, it works
      //   f1 ++ f2.map {
      //     case (k, v) => k -> (v combine f1.getOrElse(k, b.empty))
      //   }

      // the way suggested by the book
      f1 |+| f2
    }

    override def total(f: Map[K, V])(implicit m: Monoid[V]) = {
      f.values.toList.combineAll
    }

  }
}

