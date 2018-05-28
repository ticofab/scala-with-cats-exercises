package chapter1

import cats._
import cats.implicits._

object Main extends App {
  println("Hello " |+| "Cats!")
  val cat = Cat("Rufus", 8, "beige")

  // using the printable object
  import PrintableInstances._
  Printable.print(cat)

  // using the printable syntax
  import PrintableSyntax._
  cat.print

  // using cat's Show
  implicit val catShow: Show[Cat] = Show.show[Cat](cat => s"${cat.name} is a ${cat.age} year-old ${cat.color} cat.")
  println(cat.show)

  // equality exercise
  val cat1 = Cat("Garfield",   38, "orange and black")
  val cat2 = Cat("Heathcliff", 33, "orange and black")
  val optionCat1 = Option(cat1)
  val optionCat2 = Option.empty[Cat]

  implicit val catEq = Eq.instance[Cat]((cat1,cat2) => {
    val equalName = cat1.name === cat2.name
    val equalAge = cat1.age === cat2.age
    val equalColor = cat1.color === cat2.color
    equalName && equalAge && equalColor
  })

  println(cat1 === cat1) // true
  println(cat1 === cat2) // false
  println(optionCat1 === optionCat2) // false
  println(optionCat2 === none[Cat]) // true
}
