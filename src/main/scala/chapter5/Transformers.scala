package chapter5

import cats.data.EitherT
import cats.instances.all._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

object Transformers {
  type Response[A] = FutureEither[A]

  type FutureEither[A] = EitherT[Future, String, A]

  val powerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
  )

  def tacticalReport(ally1: String, ally2: String): String = {
    Await.result(canSpecialMove(ally1, ally2).value, 1.second) match {
      case Right(true) => s"$ally1 and $ally2 can kick ass."
      case Right(false) => s"$ally1 and $ally2 need more power."
      case Left(error) => error
    }
  }

  // this is the solution proposed by the book, but I can't get it to compile.
  //  def getPowerLevel(autobot: String): Response[Int] = {
  //    powerLevels.get(autobot) match {
  //      case Some(value) => EitherT.right[Future, String](Future(value))
  //      case None => EitherT.left[Future, String](Future(s"Autobot $autobot not found."))
  //    }
  //  }

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] = {
    val result = for {
      a <- getPowerLevel(ally1)
      b <- getPowerLevel(ally2)
    } yield a + b
    result.map(_ > 15)
  }

  def getPowerLevel(autobot: String): Response[Int] = {
    def result = powerLevels.get(autobot) match {
      case None => Left(s"Autobot $autobot not found.")
      case Some(value) => Right(value)
    }

    EitherT.apply(Future(result))
  }

}
