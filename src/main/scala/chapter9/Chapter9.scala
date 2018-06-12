package chapter9

import cats.Monoid
import cats.instances.all._
import cats.syntax.all._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

object Chapter9 extends App {

  val futureResult: Future[Int] = ParallelFoldMappableImpl.parallelFoldMap((1 to 1000000).toVector)(identity)
  val result = Await.result(futureResult, 1.second)

  trait FoldMappable {
    def foldMap[A, B: Monoid](sequence: Vector[A])(f: A => B): B
  }

  trait ParallelFoldMappable {
    def parallelFoldMap[A, B: Monoid](sequence: Vector[A])(f: A => B): Future[B]
  }

  object FoldMappableImpl extends FoldMappable {
    override def foldMap[A, B: Monoid](sequence: Vector[A])(f: A => B): B =
    //  sequence.map(f).foldLeft(Monoid[B].empty)(Monoid[B].combine)
      sequence.map(f).foldLeft(Monoid[B].empty)(_ |+| _)
  }

  object ParallelFoldMappableImpl extends ParallelFoldMappable {
    override def parallelFoldMap[A, B: Monoid](sequence: Vector[A])(f: A => B): Future[B] = {
      val availableCPUs = Runtime.getRuntime.availableProcessors
      println("avaible CPUs: " + availableCPUs)

      val groupSize = (1.0 * sequence.size / availableCPUs).ceil.toInt
      println("splitting sequence into groups of size: " + groupSize)

      val sequences = sequence.grouped(groupSize).toList

      Future
        .sequence(sequences.map(v => Future(FoldMappableImpl.foldMap(v)(f))))
        .map(l => l.foldLeft(Monoid[B].empty)(_ |+| _))

    }
  }

  println("result is: " + result)
}
