package dataStructures.queue

trait Queue[T] {
  def enqueue(element: T): Unit
  def dequeue(): T
  def peek(): T
  def isEmpty(): Boolean
}
