// this is for lecture 3.1
package week3

abstract class IntSet {
  def contains(value: Int): Boolean
  def +(value: Int): IntSet
  def -(value: Int): IntSet
  def ++(that: IntSet): IntSet
}

object Empty extends IntSet {
  override def contains(value: Int): Boolean = false

  override def +(value: Int): IntSet = new NonEmpty(Empty, value, Empty)

  override def -(value: Int): IntSet = Empty

  override def ++(that: IntSet): IntSet = that

  override def toString: String = "."
}

class NonEmpty(left: IntSet, element: Int, right: IntSet) extends IntSet {
  override def contains(value: Int): Boolean =
    if (value == element) true
    else if (value < element) left contains value
    else if (element < value) right contains value
    else throw new Exception("invalid value for value and element")

  // TODO : this is too inefficient
  override def +(value: Int): IntSet =
    if (value == element) this
    else if (value < element) new NonEmpty(left + value, element, right)
    else if (element < value) new NonEmpty(left, element, right + value)
    else throw new Exception("invalid value for value and element")

  // TODO : this is too inefficient
  override def -(value: Int): IntSet =
    if (value == element) left ++ right
    else if (value < element) new NonEmpty(left - value, element, right)
    else if (element < value) new NonEmpty(left, element, right - value)
    else throw new Exception("invalid value for value and element")

  // TODO : this is too inefficient
  override def ++(that: IntSet): IntSet =
    (left ++ right ++ that) + element

  override def toString: String = "{" + left + " " + element + " " + right +"}"
}