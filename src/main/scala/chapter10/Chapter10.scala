package chapter10

import cats.data.Validated
import cats.instances.all._
import cats.syntax.all._

object Chapter10 extends App {

  // testing my checkF implementation
  val af: CheckF[List[String], Int] = CheckF { v =>
    if (v > 2) v.asRight
    else List("Must be > 2").asLeft
  }

  val bf: CheckF[List[String], Int] = CheckF { v =>
    if (v < -2) v.asRight
    else List("Must be < -2").asLeft
  }

  val checkf: CheckF[List[String], Int] = af and bf

  println("my non-working solution")
  println(checkf(5))
  println(checkf(-4))
  println(checkf(1))

  // testing checkFB implementation
  val afb: CheckFB[List[String], Int] = CheckFB { v =>
    if (v > 2) v.asRight
    else List("Must be > 2").asLeft
  }

  val bfb: CheckFB[List[String], Int] = CheckFB { v =>
    if (v < -2) v.asRight
    else List("Must be < -2").asLeft
  }

  val checkfb: CheckFB[List[String], Int] = afb and bfb

  println("book solution 1")
  println(checkfb(5))
  println(checkfb(-4))
  println(checkfb(1))

  // testing book's ADT implementation with either
  val aE = PureE[List[String], Int] { v =>
    if (v > 2) v.asRight
    else List("Must be > 2").asLeft
  }

  val bE = PureE[List[String], Int] { v =>
    if (v < -2) v.asRight
    else List("Must be < -2").asLeft
  }

  val checkE: CheckE[List[String], Int] = AndE(aE, bE)

  println("book'a ADT solution")
  println(checkE(5))
  println(checkE(-4))
  println(checkE(1))

  // testing book's ADT implementation with Validated
  val a = Pure[List[String], Int] { v =>
    if (v > 2) Validated.Valid(v)
    else Validated.Invalid(List("Must be > 2"))
  }

  val b = Pure[List[String], Int] { v =>
    if (v < -2) Validated.Valid(v)
    else Validated.Invalid(List("Must be < -2"))
  }

  val check: Check[List[String], Int] = And(a, b)

  println("book'a ADT solution with Validated")
  println(check(5))
  println(check(-4))
  println(check(1))
}
