package dataStructures.deque

trait Deque[T] {
  def addFirst(element: T): Unit
  def addLast(element: T): Unit
  def removeFirst(): T
  def removeLast(): T
  def getFirst(): T
  def getLast(): T
  def isEmpty(): Boolean
}
