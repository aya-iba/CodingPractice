
object Deque {
  case class Node[T](value: Option[T], var next: Node[T] = null, var prev: Node[T] = null)
}

// O(1) TC for all methods

// with sentinel nodes
class LinkedListDeque[T]() {
  import Deque._

  private val head = Node[T](null)
  private val tail = Node[T](null)
  head.next = tail
  tail.prev = head

  def addFirst(element: T): Unit = {
    val newNode = Node[T](Option(element))

    newNode.next = head.next
    newNode.prev = head

    head.next.prev = newNode
    head.next = newNode
  }

  def addLast(element: T): Unit = {
    val newNode = Node[T](Option(element))

    newNode.prev = tail.prev
    newNode.next = tail

    tail.prev.next = newNode
    tail.prev = newNode
  }

  def removeFirst(): T = {
    if(!isEmpty()) {
      val removingValue = head.next.value.get
      head.next.next.prev = head
      head.next = head.next.next
      removingValue
    } else throw new Exception("deque is empty")
  }

  def removeLast(): T = {
    if(!isEmpty()) {
      val removingValue = tail.prev.value.get
      tail.prev.prev.next = tail
      tail.prev = tail.prev.prev
      removingValue
    } else throw new Exception("deque is empty")
  }

  def getFirst(): T = head.next.value.get

  def getLast(): T = tail.prev.value.get

  def isEmpty(): Boolean = head.next == tail

}

val deque = new LinkedListDeque[String]()

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