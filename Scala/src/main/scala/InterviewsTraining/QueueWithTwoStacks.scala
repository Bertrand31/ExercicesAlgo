import cats.implicits._

final case class Stack[T](data: List[T] = Nil) {

  def insert(elem: T): Stack[T] =
    new Stack(elem :: this.data)

  def pop: Option[(T, Stack[T])] =
    data match {
      case Nil => None
      case head :: rest => Some((head, new Stack(rest)))
    }
}

final case class QueueWithTwoStacks[T](
  stack: Stack[T] = Stack[T](),
  reverseStack: Stack[T] = Stack[T]()
) {

  def insert(elem: T): QueueWithTwoStacks[T] =
    new QueueWithTwoStacks(
      this.stack.insert(elem),
      this.stack.data.foldLeft(new Stack(elem :: Nil))(_.insert(_))
    )

  def pop: Option[(T, QueueWithTwoStacks[T])] =
    this.reverseStack.pop.map({
      case (elem, newReverseStack) =>
        (
          elem,
          new QueueWithTwoStacks(
            newReverseStack.data.foldLeft(new Stack())(_.insert(_)),
            newReverseStack,
          ),
        )
    })
}

object QueueWithTwoStacks extends App {

  private val sampleData = Seq[Int](8, 7, 4)

  val testQueue = sampleData.foldLeft(new QueueWithTwoStacks[Int]())(_.insert(_))

  assert(testQueue.pop.get._1 === 8)
}
