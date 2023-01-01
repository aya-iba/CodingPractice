import dataStructures.queue.Queue

object ArrayQueue {
  var Capacity = 2
}
class ArrayQueue[T]() extends Queue[T] {
  import ArrayQueue._

  private var queue = Array.fill[Option[T]](Capacity)(None)
  private var head = 0
  private var tail = 0
  private var count = 0

  // have two pointers: head and tail. head points to the beg of the queue. tail points to the end.

  private def isFull(): Boolean = count == Capacity // TC: O(1)

  private def doubleCapacity(): Unit = {
    Capacity = Capacity * 2

    val newQueue = Array.fill[Option[T]](Capacity)(None)
    Array.copy(queue, head, newQueue, 0, queue.size - head)
    Array.copy(queue, 0, newQueue, queue.size - head, head)

    queue = newQueue
    head = 0
    tail = count - 1
    println("doubled capacity. new size is " + Capacity)
  }

  // resizing
  // copy old array to new when capacity is reached. copy head to end of array & beginning to tail
  def enqueue(element: T): Unit = { // TC: O(1)
    if (isEmpty) {
      queue(head) = Option(element)
      count += 1
    } else {
      if(isFull()) doubleCapacity()

      if(tail == Capacity - 1) {
        tail = 0
      } else {
        tail += 1
      }

      queue(tail) = Option(element)
      count += 1
    }
  }

  def dequeue(): T = // TC: O(1)
    if(!isEmpty()) {
      val prevVal = queue(head).get
      queue(head) = None

      if(head == count - 1) {
        head = 0
      } else {
        head += 1
      }

      count -= 1
      prevVal
    } else throw new Exception("queue is empty")

  def peek(): T =  // TC: O(1)
    if (!isEmpty) queue(head).get else throw new Exception("queue is empty")

  def isEmpty(): Boolean = queue(head) == None // TC: O(1)
}

val queue = new ArrayQueue[String]()

queue.enqueue("string 1")
queue.enqueue("string 2")
queue.peek() // val res2: String = string 1
queue.dequeue() // val res3: String = string 1
queue.peek() // val res4: String = string 2
queue.enqueue("string 3")
queue.dequeue() // val res6: String = string 2
queue.enqueue("string 4")
queue.enqueue("string 5") // doubled capacity. new size is 4
queue.enqueue("string 6")
queue.dequeue() // val res10: String = string 3
