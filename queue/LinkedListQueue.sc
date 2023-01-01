import dataStructures.queue.Queue

// without sentinel nodes
object LinkedListQueue {
  case class Node[T](value: T, var next: Node[T] = null)
}

class LinkedListQueue[T]() extends Queue[T] {
  import LinkedListQueue._

  private var head: Node[T] = null
  private var tail: Node[T] = null

  def enqueue(element: T): Unit = { // TC: O(1)
    val newNode = Node(element)
    if(isEmpty()) {
      head = newNode
      tail = newNode
    } else {
      tail.next = newNode
      tail = newNode
    }
  }

  def dequeue(): T = { // TC: O(1)
    if(!isEmpty()) {
      val prevHeadValue = head.value
      head = head.next
      prevHeadValue
    }
    else throw new Exception("queue is empty")
  }

  def peek(): T =
    if(!isEmpty()) head.value else throw new Exception("queue is empty") // TC: O(1)

  def isEmpty(): Boolean = head == null // TC: O(1)
}

val queue = new LinkedListQueue[String]()

queue.enqueue("string 1")
queue.enqueue("string 2")
queue.isEmpty() // val res2: Boolean = false
queue.peek() // val res3: String = string 1
queue.dequeue() // val res4: String = string 1
queue.dequeue() // val res5: String = string 2
queue.isEmpty() // val res6: Boolean = true
queue.enqueue("string 3")
queue.enqueue("string 4")
queue.dequeue() // val res9: String = string 3
queue.dequeue() // val res10: String = string 4

// with sentinel node just at the head
/*
object LinkedListQueue {
  // requires `prev` attribute.
  case class Node(value: String, var next: Node = null, var prev: Node = null)
}

class LinkedListQueue() {
  import LinkedListQueue._

  private val head: Node = Node(null)
  private val tail: Node = Node(null)
  head.next = tail
  tail.prev = head

  // no need to worry whether new node is the first one added to the queue
  def enqueue(element: String): Unit = {
      val newNode = Node(element)
      tail.prev.next = newNode
      newNode.prev = tail.prev
      tail.prev = newNode
      newNode.next = tail
  }

  def dequeue(): String = {
    if(!isEmpty()) {
      val removingNode = head.next
      head.next.next.prev = head
      head.next = head.next.next
      removingNode.next = null
      removingNode.prev = null
      removingNode.value
    } else throw new Exception("queue is empty")
  }

  def peek(): String = head.next.value

  def isEmpty(): Boolean = head.next == tail
}
 */


val queue = new LinkedListQueue()

queue.enqueue("string 1")
queue.enqueue("string 2")
queue.isEmpty() // val res2: Boolean = false
queue.peek() // val res3: String = string 1
queue.dequeue() // val res4: String = string 1
queue.dequeue() // val res5: String = string 2
queue.isEmpty() // val res6: Boolean = true
queue.enqueue("string 3")
queue.enqueue("string 4")
queue.dequeue() // val res9: String = string 3
queue.dequeue() // val res10: String = string 4

