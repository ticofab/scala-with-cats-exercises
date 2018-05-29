package chapter3

import cats.Functor

sealed trait Tree[+A]

final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

final case class Leaf[A](value: A) extends Tree[A]

object Tree {
  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)

  def leaf[A](value: A): Tree[A] = Leaf(value)
}

object BranchingFunctor {

  //  implicit val treeShow = Show[Tree].show {
  //    case Branch(l, r) => "branchl + branchr"
  //    case Leaf(value) => "leaf"
  //  }

  implicit val treeFunctor: Functor[Tree] = {
    new Functor[Tree] {
      override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = {
        fa match {
          case Branch(l, r) => Branch(map(l)(f), map(r)(f))
          case Leaf(value) => Leaf[B](f(value))
        }
      }
    }
  }

  //  implicit val branchFunctor: Functor[Branch] = {
  //    new Functor[Branch] {
  //      override def map[A, B](fa: Branch[A])(f: A => B): Branch[B] =
  //        Branch(map(fa.left)(f), map(fa.right)(f))
  //    }
  //  }
  //
  //  implicit val leafFunctor: Functor[Leaf] = {
  //    new Functor[Leaf] {
  //      override def map[A, B](fa: Leaf[A])(f: A => B): Leaf[B] = Leaf(f(fa.value))
  //    }
  //  }
}
