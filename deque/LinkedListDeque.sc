
object Deque {
  case class Node(value: String, var next: Node = null, var prev: Node = null)
}

// O(1) TC for all methods

// with sentinel nodes
class LinkedListDeque() {
  import Deque._

  private val head = Node(null)
  private val tail = Node(null)
  head.next = tail
  tail.prev = head

  def addFirst(element: String): Unit = {
    val newNode = Node(element)

    newNode.next = head.next
    newNode.prev = head

    head.next.prev = newNode
    head.next = newNode
  }

  def addLast(element: String): Unit = {
    val newNode = Node(element)

    newNode.prev = tail.prev
    newNode.next = tail

    tail.prev.next = newNode
    tail.prev = newNode
  }

  def removeFirst(): String = {
    if(!isEmpty()) {
      val removingValue = head.next.value
      head.next.next.prev = head
      head.next = head.next.next
      removingValue
    } else throw new Exception("deque is empty")
  }

  def removeLast(): String = {
    if(!isEmpty()) {
      val removingValue = tail.prev.value
      tail.prev.prev.next = tail
      tail.prev = tail.prev.prev
      removingValue
    } else throw new Exception("deque is empty")
  }

  def getFirst(): String = head.next.value

  def getLast(): String = tail.prev.value

  def isEmpty(): Boolean = head.next == tail

}

val deque = new LinkedListDeque()

deque.isEmpty() // val res0: Boolean = true
deque.addFirst("string 1")
deque.isEmpty() // val res2: Boolean = false
deque.addFirst("string 2")
deque.getFirst() // val res4: String = string 2
deque.getLast() // val res5: String = string 1
deque.removeFirst() // val res6: String = string 2
deque.removeFirst() // val res7: String = string 1
deque.addLast("string 3")
deque.addLast("string 4")
deque.removeLast() // val res10: String = string 4
deque.removeLast() // val res11: String = string 3