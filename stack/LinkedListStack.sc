import dataStructures.stack.Stack

object LinkedListStack {
  case class Node[T](value: T, var next: Node[T] = null)
}

// without sentinel node
// templatized
class LinkedListStack[T]() extends Stack[T]{
  import LinkedListStack._

  private var head: Node[T] = null

  // TC: O(1)
  def push(element: T): Unit = {
    val newNode = Node(element)
    if(isEmpty()) {
      head = newNode
    } else {
      newNode.next = head
      head = newNode
    }
  }

  // TC: O(1)
  def pop(): T = {
    if(!isEmpty()) {
      if(head.next != null) {
        val removingValue = head.value
        head = head.next
        removingValue
      } else {
        val removingValue = head.value
        head = null
        removingValue
      }
    } else throw new Exception("stack is empty")
  }

  // TC: O(1)
  def peek(): T = {
    if(!isEmpty()) head.value
    else throw new Exception("stack is empty")
  }

  // TC: O(1)
  def isEmpty(): Boolean = head == null

}




// with sentinel node
// cannot make data type generic with sentinel nodes because there needs to be a consistent default value
// e.g. string/char can default to null while int/double cannot.
/*
class LinkedListStack() {
  import LinkedListStack._

  private val head = Node(null)

  // TC: O(1)
  def push(element: String): Unit = {
    val newNode = Node(element)
    newNode.next = head.next
    head.next = newNode
  }

  // TC: O(1)
  def pop(): String = {
    if(!isEmpty()) {
      val removingValue = head.next.value
      head.next = head.next.next
      removingValue
    } else throw new Exception("stack is empty")
  }

  // TC: O(1)
  def peek(): String = {
    if(!isEmpty()) head.next.value
    else throw new Exception("stack is empty")
  }

  // TC: O(1)
  def isEmpty(): Boolean = head.next == null

}
 */

val stack = new LinkedListStack[Int]()

stack.isEmpty() // val res0: Boolean = true
stack.push(1)
stack.isEmpty() // val res2: Boolean = false
stack.push(2)
stack.peek() // val res4: Int = 2
stack.pop() // val res5: Int = 2
stack.push(3)
stack.pop() // val res7: Int = 3
stack.pop() // val res8: Int = 1