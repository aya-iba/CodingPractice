
object LinkedListStack {
  case class Node(value: String, var next: Node = null)
}

// without sentinel node

class LinkedListStack() {
  import LinkedListStack._

  private var head: Node = null

  // TC: O(1)
  def push(element: String): Unit = {
    val newNode = Node(element)
    if(isEmpty()) {
      head = newNode
    } else {
      newNode.next = head
      head = newNode
    }
  }

  // TC: O(1)
  def pop(): String = {
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
  def peek(): String = {
    if(!isEmpty()) head.value
    else throw new Exception("stack is empty")
  }

  // TC: O(1)
  def isEmpty(): Boolean = head == null

}




// with sentinel node
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

val stack = new LinkedListStack()

stack.isEmpty()
stack.push("string 1")
stack.isEmpty()
stack.push("string 2")
stack.peek()
stack.pop()
stack.push("string 3")
stack.pop()
stack.pop()