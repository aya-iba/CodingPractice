import dataStructures.stack.Stack

import scala.collection.mutable
import scala.reflect.ClassTag

object ArrayStack {
  private var Capacity = 10
}

// ClassTag stores the element type of a generic collection for runtime use. At runtime, the type
// is erased and there needs to be a way to remember it, with one strategy being the usage of a ClassTag.
// [T: ClassTag], as written below, is syntactic sugar for an implicit ClassTag parameter. When this parameter is present, the compiler
// automatically generates a ClassTag object with the passed in type.

class ArrayStack[T: ClassTag]() extends Stack[T]{
  import ArrayStack._

  private var stack = new Array[T](Capacity)
  private var count = 0
  private val lastIdx = () => count - 1

  private def isFull(): Boolean = count >= Capacity
  private def doubleStackCapacity(): Unit = {
    Capacity = Capacity * 2
    val newStack = new Array[T](Capacity)
    stack.copyToArray(newStack, 0, stack.size)
    stack = newStack
    println("doubled size")
  }

  // TC: O(1) amortized
  def push(element: T): Unit = {
    if(isFull()) doubleStackCapacity()
    stack(lastIdx() + 1) = element
    count += 1
  }

  // TC: O(N - 1)
  def pop(): T = {
    // if stack is not empty we can remove an item
    if (!isEmpty()) {
      val removingItem = stack(lastIdx())
      Array.copy(stack, 0, stack, 0, lastIdx())
      count -= 1
      removingItem
    } else throw new Exception("stack is empty")
  }

  // TC: O(1)
  def peek(): T =
    if(!isEmpty()) {
      stack(lastIdx())
    } else throw new Exception("stack is empty")

  // TC: 0(1)
  def isEmpty(): Boolean = count == 0
}


val stack = new ArrayStack[String]()

stack.isEmpty() // val res0: Boolean = true
stack.push("string 1")
stack.isEmpty() // val res2: Boolean = false
stack.push("string 2")
stack.peek() // val res4: String = string 2
stack.pop() // val res5: String = string 2
stack.push("string 3")
stack.push("string 4")
stack.push("string 5") // doubled size
stack.pop() // val res7: String = string 5
stack.pop() // val res8: String = string 4

val intStack = new ArrayStack[Int]()
intStack.push(6)
intStack.push(7)
intStack.pop() // val res15: Int = 7
intStack.pop() // val res16: Int = 6
