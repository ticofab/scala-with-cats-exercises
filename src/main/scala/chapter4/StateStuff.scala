package chapter4

object StateStuff {

  import cats.data.State

  type CalcState[A] = State[List[Int], A]

  def evalInput(input: String) = {
    evalAll(input.toList.map(_.toString)).run(Nil).value
  }

  def evalAll(input: List[String]): CalcState[Int] = {
    val opList = input.map(s => evalOneF(s))
    val chain = opList.fold(State.pure[List[Int], Int](0)) {
      (previous, current) =>
        previous.flatMap(_ => current)
    }
    chain
  }

  def evalOneF(sym: String): CalcState[Int] =
    State[List[Int], Int] {
      oldStack => {

        def operation(func: (Int, Int) => Int, oldStack: List[Int]) = {
          val resultingInt = func(oldStack.head, oldStack.tail.head)
          (resultingInt :: oldStack.drop(2), resultingInt)
        }

        sym match {
          case "+" => operation(_ + _, oldStack)
          case "-" => operation((x, y) => x - y, oldStack) // equivalent
          case "*" => operation(_ * _, oldStack)
          case "/" => operation(_ / _, oldStack)
          case n => (n.toInt :: oldStack, n.toInt)
        }

      }
    }


}
