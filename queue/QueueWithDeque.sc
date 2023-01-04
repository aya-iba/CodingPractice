import dataStructures.deque.ArrayDeque
import dataStructures.queue.Queue
import scala.reflect.ClassTag
class QueueD[T: ClassTag]() extends Queue[T] {

  val queue = new ArrayDeque[T]

  def enqueue(element: T): Unit = queue.addLast(element)

  def dequeue(): T = queue.removeFirst()

  def peek(): T = queue.getFirst()

  def isEmpty(): Boolean = queue.isEmpty()
}

val queue = new QueueD[String]()

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