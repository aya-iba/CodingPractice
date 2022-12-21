import scala.collection.mutable._

class ArrayStack() {

  private val stack = new ArrayBuffer[String](4)

  // when using an array buffer, TC for push and pop are O(N) because a new array is created each time.
  // however if the array is static we can have a variable that keeps track of the top idx and
  // update the idx value at time of removal or addition. that would result in O(1) TC.

  // TC: O(N) ?
  def push(element: String): Unit = {
    stack.append(element)
  }

  // TC: O(N)
  def pop(): String = {
    if (!isEmpty()) {
      val removingItem = stack(stack.length - 1)
      stack.remove(stack.length - 1) // TC: stack.remove == O(N). copies elements to new array
      removingItem
    } else throw new Exception("stack is empty")
  }

  // TC: O(1)
  def peek(): String =
    if(!isEmpty()) {
      stack(stack.length - 1)
    } else throw new Exception("stack is empty")

  // TC: 0(1)
  def isEmpty(): Boolean = stack.length == 0
}


val stack = new ArrayStack()

stack.isEmpty()
stack.push("string 1")
stack.isEmpty()
stack.push("string 2")
stack.peek()
stack.pop()
stack.push("string 3")
stack.pop()
stack.pop()