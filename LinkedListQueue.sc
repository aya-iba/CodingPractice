// without sentinel nodes

object LinkedListQueue {
  case class Node(value: String, var next: Node = null)
  private val capacity = 3
}

class LinkedListQueue() {
  import LinkedListQueue._
  private var head: Node = null
  private var tail: Node = null
  private var count = 0

  def enqueue(element: String): Boolean = { // TC: O(1)
    val newNode = Node(element)
    if(isEmpty()) {
      head = newNode
      tail = newNode
      count += 1
      true
    } else if(!isFull()) {
      tail.next = newNode
      tail = newNode
      count += 1
      true
    } else false
  }

  def dequeue(): String = { // TC: O(1)
    if(!isEmpty()) {
      val prevHeadValue = head.value
      head = head.next
      count -= 1
      prevHeadValue
    }
    else throw new Exception()
  }

  def peek(): String =
    if(!isEmpty()) head.value else throw new Exception() // TC: O(1)

  def isFull(): Boolean = count == capacity // TC: O(1)

  def isEmpty(): Boolean = head == null // TC: O(1)

}


// with sentinel nodes

object LinkedListQueue {
  // requires `prev` attribute.
  case class Node(value: String, var prev: Node = null, var next: Node = null)
  private val capacity = 3
}

class LinkedListQueue() {
  import LinkedListQueue._

  private val head: Node = Node(null)
  private val tail: Node = Node(null)
  head.next = tail
  tail.prev = head

  private var count = 0

  // no need to worry whether new node is the first one added to the queue
  def enqueue(element: String): Boolean = {
    if(!isFull()) {
      val newNode = Node(element)
      tail.prev.next = newNode
      newNode.prev = tail.prev
      tail.prev = newNode
      newNode.next = tail
      count += 1
      true
    } else false
  }

  def dequeue(): String = {
    if(!isEmpty()) {
      val removingNode = head.next
      head.next.next.prev = head
      head.next = head.next.next
      removingNode.next = null
      removingNode.prev = null
      count -= 1
      removingNode.value
    } else throw new Exception()
  }

  // no need to check for emptiness bc if it's empty the head will be pointing to sentinel tail
  def peek(): String = head.next.value

  def isFull(): Boolean = count == capacity

  def isEmpty(): Boolean = head.next == tail

}


val queue = new LinkedListQueue()

queue.isFull()
queue.isEmpty()
queue.enqueue("string 1")
queue.enqueue("string 2")
queue.isEmpty()
queue.isFull()
queue.peek()
queue.dequeue()
queue.dequeue()
queue.isEmpty()
queue.peek()
queue.enqueue("string 3")
queue.peek()
queue.dequeue()
queue.enqueue("string 4")
queue.enqueue("string 5")
queue.enqueue("string 6")
queue.isFull()

