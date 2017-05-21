//this is for lecture 4.2 Functions as Objects
trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

class Nil[T] extends List[T] {
  def isEmpty = true
  def head = throw new NoSuchElementException("Nil.head")
  def tail = throw new NoSuchElementException("Nil.tail")
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
}

object List {
  //def List() = new Nil
  def apply[T]() = new Nil

  //def List[T](x: T) = new Cons(x, new Nil)
  def apply[T](x: T) = new Cons(x, new Nil)

  //def List[T](x: T, y: T) = new Cons(x, new Cons(y, new Nil))
  def apply [T](x: T, y: T) = new Cons(x, new Cons(y, new Nil))
}


List()
List(2)
List(3,4)