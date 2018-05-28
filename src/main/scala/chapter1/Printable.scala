package chapter1

case class Cat(name: String, age: Int, color: String)

trait Printable[A] {
  def format(value: A): String
}

object PrintableInstances {

  implicit val stringPrinter: Printable[String] = (value: String) => value

  implicit val intPrinter: Printable[Int] = (value: Int) => value.toString

  implicit val catPrinter: Printable[Cat] = (value: Cat) => {
    val nameS = Printable.format(value.name)
    val ageS = Printable.format(value.age)
    val colorS = Printable.format(value.color)
    s"$nameS is a $ageS year-old $colorS cat."
  }
}

object Printable {
  def format[A](value: A)(implicit valuePrinter: Printable[A]) = valuePrinter.format(value)

  def print[A](value: A)(implicit valuePrinter: Printable[A]) = println(valuePrinter.format(value))
}

object PrintableSyntax {

  implicit class PrintableOps[A](value: A) {
    def format(implicit printable: Printable[A]): String = printable.format(value)

    def print(implicit printable: Printable[A]): Unit = println(printable.format(value))
  }

}




