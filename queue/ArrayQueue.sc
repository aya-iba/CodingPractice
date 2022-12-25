
class ArrayQueue() {
  private val arrSize = 3
  private val arr = Array.ofDim[String](arrSize)
  private var head = 0
  private var tail = 0

  // have two pointers: head and tail. head points to the beg of the queue. tail points to the end.
  // check if full. if not, add new element one right to the tail. reassign tail to where that new element is.


  // resizing
  // copy old array to new when capacity is reached. copy head to end of array & beginning to tail
  def enqueue(element: String): Boolean = { // TC: O(1)
    if (isEmpty) {
      arr(head) = element
      true
    }
    else if(!isFull()) {
      if(tail == arrSize - 1) {
        tail = 0
        arr(tail) = element
      } else {
        tail += 1
        arr(tail) = element
      }
      true
    } else
      false
  }

  // check if empty. if not empty, set arr[idx] to null and move head one over to the right.
  def dequeue(): String = // TC: O(1)
    if(!isEmpty()) {
      val prevVal = arr(head)
      arr(head) = null
      head += 1
      prevVal
    } else throw new Exception()

  // check if empty. if not, return head
  def peek(): String =  // TC: O(1)
    if (!isEmpty) arr(head) else throw new Exception()

  // full when tail is at the last element or head - tail == 1
  def isFull(): Boolean = {tail == (arrSize - 1) && head == 0} || (head - tail) == 1 // TC: O(1)

  // empty when head == null
  def isEmpty(): Boolean = arr(head) == null // TC: O(1)
}

val queue = new ArrayQueue()
queue.isFull()
queue.isEmpty()
queue.enqueue("string 1")
queue.enqueue("string 2")
queue.isEmpty()
queue.isFull()
queue.peek()
queue.dequeue()
queue.peek()
queue.enqueue("string 3")
queue.dequeue()
queue.enqueue("string 4")
queue.enqueue("string 5")
queue.enqueue("string 6")
queue.dequeue()
