import dataStructures.deque.Deque

import scala.reflect.ClassTag

object ArrayDeque {
  private var Capacity = 2
}
class ArrayDeque[T: ClassTag] extends Deque[T] {
  import ArrayDeque._

  private var deque = Array.fill[Option[T]](Capacity)(None)
  private var count = 0
  private var first = 0
  private var last = deque.size - 1

  private def isFull(): Boolean = count == Capacity
  private def doubleCapacity(): Unit = {
    Capacity = Capacity * 2

    val newDeque = Array.fill[Option[T]](Capacity)(None)
    Array.copy(deque, first, newDeque, 0, deque.size - first)
    Array.copy(deque, 0, newDeque, deque.size - first, first)

    deque = newDeque
    first = 0
    last = count - 1
    println("doubled capacity. new size is " + Capacity)
  }

  def addFirst(element: T): Unit = {
    if (isFull) doubleCapacity

//    first = (deque.size + first - 1) % deque.size
    first = (first - 1) & deque.size - 1

    deque(first) = Option(element)
    count += 1
  }

  def addLast(element: T): Unit = {
    if (isFull) doubleCapacity

    // last = (last + 1) % deque.size
    last = (last + 1) & deque.size - 1

    deque(last) = Option(element)
    count += 1
  }

  def getFirst(): T = {
    if (!isEmpty) deque(first).get else throw new Exception("deque is empty")
  }

  override def getLast(): T = {
    if (!isEmpty) deque(last).get else throw new Exception("deque is empty")
  }

  override def isEmpty(): Boolean = count == 0

  // using option to avoid creating a copy just to remove values that are not on the ends
  override def removeFirst(): T = {
    if (!isEmpty) {
      val removingValue = deque(first).get // value should be present unless there is s mistake in the logic
      deque(first) = None

      // first = (first + 1) % deque.size
      first = (first + 1) & deque.size - 1

      count -= 1
      removingValue
    } else throw new Exception("deque is empty")
  }

  override def removeLast(): T = {
    if (!isEmpty) {
      val removingValue = deque(last).get
      deque(last) = None

//      last = (deque.size + last - 1) % deque.size
      last = (last - 1) & deque.size - 1

      count -= 1
      removingValue
    }
    else throw new Exception("deque is empty")
  }
}

val deque = new ArrayDeque[String]()

deque.addLast("string 1")
deque.addLast("string 2")
deque.addLast("string 3") //doubled capacity. new size is 4
deque.removeLast() // val res3: String = string 3
deque.removeLast() // val res4: String = string 2
deque.removeLast() // val res5: String = string 1

deque.addLast("string 1")
deque.addLast("string 2")
deque.addLast("string 3")

deque.addLast("string 4")
deque.addLast("string 5") // doubled capacity. new size is 8

deque.getLast() // val res11: String = string 5
deque.getFirst() // val res12: String = string 1

