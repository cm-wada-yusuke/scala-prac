package dwango

//trait Stack[+T] {
//  def pop: (T, Stack[T])
//  def push[E >: T](e: E): Stack[E]
//  def isEmpty: Boolean
//}
//
//class NonEmptyStack[+T](private val top: T, private val rest: Stack[T]) extends Stack[T] {
//  def push[E >: T](e: E): Stack[E] = new NonEmptyStack[E](e, this)
//  def pop: (T, Stack[T]) = (top, rest)
//  def isEmpty: Boolean = false
//}
//
//object EmptyStack extends Stack[Nothing] {
//  def pop: Nothing = throw new IllegalArgumentException("empty stack")
//  def push[E >: Nothing](e: E): Stack[E] = new NonEmptyStack[E](e, this)
//  def isEmpty: Boolean = true
//}
//
//object Stack {
//  def apply(): Stack[Nothing] = EmptyStack
//}

trait Stack[+A] {
  def push[E >: A](e: E): Stack[E]

  def top: A

  def pop: Stack[A]

  def isEmpty: Boolean
}

class NonEmptyStack[+A](private val first: A, private val rest: Stack[A]) extends Stack[A] {
  def push[E >: A](e: E): Stack[E] = new NonEmptyStack[E](e, this)

  def top: A = ???

  def pop: Stack[A] = ???

  def isEmpty: Boolean = ???
}

case object EmptyStack extends Stack[Nothing] {
  def push[E >: Nothing](e: E): Stack[E] = new NonEmptyStack[E](e, this)

  def top: Nothing = throw new IllegalArgumentException("empty stack")

  def pop: Nothing = throw new IllegalArgumentException("empty stack")

  def isEmpty: Boolean = true
}

object Stack {
  def apply(): Stack[Nothing] = EmptyStack
}

object HelloStack {
  def main(args: Array[String]) {
    val stack = Stack()
    println(stack.isEmpty)

    val stack1 = stack.push("first element")
    println(stack1.isEmpty)
    println(stack1.pop)

    val stack2 = stack1.push(2)
    println(stack2.isEmpty)
    println(stack2.top)
    println(stack2.pop.top)
  }
}