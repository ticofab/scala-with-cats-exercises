package chapter2

import BooleanMonoids._

object Chapter2 extends App {
  println(BooleanMonoids(and).combine(true, true)) // true
  println(BooleanMonoids(or).combine(false, true)) // true
  println(BooleanMonoids(xor).combine(true, true)) // false
  println(BooleanMonoids(nxor).combine(false, true)) // false
}
