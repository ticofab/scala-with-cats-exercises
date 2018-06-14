package chapter10

import cats.Semigroup
import cats.data.Validated
import cats.syntax.all._

// my own implementation (doesn't work)
case class CheckF[E: Semigroup, A](f: A => Either[E, A]) {
  def apply(value: A) = f(value)

  def and(that: CheckF[E, A]): CheckF[E, A] =
    CheckF { a =>

      for {
        _ <- f(a)
        thatCheck <- that.f(a)
      } yield thatCheck

    }

}

// the book's implementation (works)
case class CheckFB[E: Semigroup, A](f: A => Either[E, A]) {
  def apply(value: A) = f(value)

  def and(that: CheckFB[E, A]): CheckFB[E, A] = CheckFB { a =>
    (this (a), that(a)) match {
      case (Left(e1), Left(e2)) => (e1 |+| e2).asLeft
      case (Left(e), Right(_)) => e.asLeft
      case (Right(_), Left(e)) => e.asLeft
      case (Right(_), Right(_)) => a.asRight
    }
  }
}

// the book's ADT implementation with Either
sealed trait CheckE[E, A] {

  def and(that: CheckE[E, A]): CheckE[E, A] = AndE(this, that)

  def apply(a: A)(implicit s: Semigroup[E]): Either[E, A] = this match {

    case PureE(func) => func(a)

    case AndE(leftOp, rightOp) =>
      (leftOp(a), rightOp(a)) match {
        case (Left(e1), Left(e2)) => (e1 |+| e2).asLeft
        case (Left(e), Right(_)) => e.asLeft
        case (Right(_), Left(e)) => e.asLeft
        case (Right(_), Right(_)) => a.asRight
      }
  }
}

case class AndE[E, A](leftOp: CheckE[E, A],
                      rightOp: CheckE[E, A]) extends CheckE[E, A]

case class PureE[E, A](func: A => Either[E, A]) extends CheckE[E, A]


// the book's ADT implementation with Validated, changed with an implicit parameter Semigroup[A]
sealed trait Check[E, A] {

  def and(that: Check[E, A]): Check[E, A] = And(this, that)

  def or(that: Check[E, A]): Check[E, A] = Or(this, that)

  def apply(a: A)(implicit s: Semigroup[E], sa: Semigroup[A]): Validated[E, A] = this match {

    case Pure(func) => func(a)

    case And(left, right) => left(a).combine(right(a))

    case Or(left, right) =>
      val v1: Validated[E, A] = left(a)
      val v2: Validated[E, A] = right(a)
      if (v1.isValid || v2.isValid) Validated.Valid(a)
      else v1.combine(v2)

  }
}

case class And[E, A](left: Check[E, A],
                     right: Check[E, A]) extends Check[E, A]

case class Pure[E, A](func: A => Validated[E, A]) extends Check[E, A]

case class Or[E, A](left: Check[E, A],
                    right: Check[E, A]) extends Check[E, A]
