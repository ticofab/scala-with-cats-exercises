package chapter11

import cats.instances.all._

object Chapter11 extends App {
  // test from book
  val g1 = Map("a" -> 7, "b" -> 3)
  val g2 = Map("a" -> 2, "b" -> 5)
  val counter = GCounter2[Map, String, Int]
  val merged = counter.merge(g1, g2)
  // merged: Map[String,Int] = Map(a -> 7, b -> 5)
  val total = counter.total(merged)
  // total: Int = 12

  println(merged)
  println(total)
}
