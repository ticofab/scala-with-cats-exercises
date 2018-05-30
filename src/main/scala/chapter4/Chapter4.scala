package chapter4

import cats._
import cats.data.Writer
import cats.instances.all._
import cats.syntax.all._

object Chapter4 extends App {

  val opt1 = Monad[Option].pure(3)
  val opt2 = Monad[Option].flatMap(opt1)(a => Some(a + 2))
  val opt3 = Monad[Option].map(opt2)(a => 100 * a)

  val list1 = Monad[List].pure(3)
  val list2 = Monad[List].flatMap(List(1, 2, 3))(a => List(a, a * 10))
  val list3 = Monad[List].map(list2)(a => a + 123)
  println(opt1)
  println(opt2)
  println(opt3)
  println(list1)
  println(list2)
  println(list3)

  val list4 = Monad[List].pure(List(3, 5, 6))
  println(list4.show)

  val m̦yId = 3: Id[Int]
  println(m̦yId.show)

  println(1.pure[Option])
  println(1.pure[List])
  println((1, 2, 3).pure[List])
  val w1: Writer[Vector[String], Int] = 1.writer(Vector("value is 1"))

  println(sumSquare(Option(3), Option(4)))
  println(sumSquare(List(1), List(2)))
  println(sumSquare(Option(2), Option(2)))
  val w2: Writer[Vector[String], Int] = 2.writer(Vector("value is 2"))
  // chaining writers using for comprehension
  val w5 = for {
    a <- w1
    b <- w2
  } yield a + b
  println(w1)
  println(w2)
  // same as w5, de-sugared
  val w6 = w1.flatMap(a => w2.map(b => a + b))
  println(w5)

  def sumSquare[F[_] : Monad](a: F[Int], b: F[Int]): F[Int] = a.flatMap(x => b.map(y => x * x + y * y))

  println(w6)

  // factorial example
  def factorial(n: Int): Int = {
    val ans = if (n == 0) 1 else n * factorial(n - 1)
    println(s"fact $n $ans")
    ans
  }

}

