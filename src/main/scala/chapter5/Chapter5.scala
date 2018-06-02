package chapter5

import cats.data.OptionT
import cats.instances.all._
import cats.syntax.all._

// monad transformers
object Chapter5 extends App {
  type ListOption[A] = OptionT[List, A]
  // EitherOption
  type ErrorOr[A] = Either[String, A]
  type ErrorOrOption[A] = OptionT[ErrorOr, A]
  println(result1)
  println(result2)
  // ListOption
  val result1: ListOption[Int] = OptionT(List(Option(10), None, Option(30)))
  println(result3)
  val result2: ListOption[Int] = 32.pure[ListOption]
  println(result4)
  val result3 = result1.flatMap { x: Int =>
    result2.map { y: Int =>
      x + y
    }
  }
  val result4 = for {
    a <- result1
    b <- result2
  } yield a + b
  val a = 10.pure[ErrorOrOption]
  val b = 32.pure[ErrorOrOption]
  val c = a.flatMap(x => b.map(y => x + y))
  println(a)
  println(b)
  println(c)

  val d = OptionT[ErrorOr, Int](Left("A terrible error."))
  val e = OptionT[ErrorOr, Int](Right(None))
  println(d)
  println(e)
  val f = for {
    e1 <- d
    e2 <- e
  } yield e1 + e2
  println(f)

  val g = 10.pure[ErrorOrOption]
  val h = OptionT[ErrorOr, Int](Right(None))
  val i = for {
    e1 <- g
    e2 <- h
  } yield e1 + e2
  println(i)

  val e1 = for {
    a <- Right(34)
    b <- Right(30)
  } yield a + b

  val e2 = for {
    a <- Right(34): Either[String, Int]
    b <- Left("terrible!"): Either[String, Int]
  } yield a + b

  println(e1)
  println(e2)

  // transformers exercise
  import Transformers._

  println(tacticalReport("Jazz", "Bumblebee"))
  println(tacticalReport("Hot Rod", "Bumblebee"))
  println(tacticalReport("Jazz", "Ironhide"))

}

