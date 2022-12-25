package dataStructures.stack

trait Stack[T] {
  def push(element: T): Unit
  def pop: T
  def peek: T
  def isEmpty: Boolean
}
