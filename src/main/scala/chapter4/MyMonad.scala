package chapter4

import cats.Id

import scala.language.higherKinds

trait MyMonad[F[_]] {
  def pure[A](a: A): F[A]

  def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]

  // these two versions of map should be equivalent
  def map[A, B](value: F[A])(func: A => B): F[B] = flatMap(value)(func andThen pure)

  def maq[A, B](value: F[A])(func: A => B): F[B] = flatMap(value)(a => pure(func(a)))
}

object IdImpl {
  def pure[A](a: A): Id[A] = a

  def flatMap[A, B](a: A)(func: A => Id[B]): Id[B] = func(a)

  def map[A, B](a: Id[A])(func: A => B): Id[B] = func(a)
}


