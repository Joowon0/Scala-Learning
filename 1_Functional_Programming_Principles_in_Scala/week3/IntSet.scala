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
  // Is using this function readable?
  private def traverse[A](value: Int, base: A, leftTraverse: A, rightTraverse: A): A =
    if (value == element) base
    else if (value < element) leftTraverse
    else if (element < value) rightTraverse
    else throw new Exception("invalid value for value and element")

  override def contains(value: Int): Boolean =
    traverse(value, true, left contains value, right contains value)

  
  private def valueHandler(f: (IntSet, Int) => IntSet, value: Int, base: IntSet) =
    traverse(value, base,
      new NonEmpty(f(left, value), element, right),
      new NonEmpty(left, element, f(right, value)))

  override def +(value: Int): IntSet = valueHandler(_+_, value, this)

  override def -(value: Int): IntSet = valueHandler(_-_, value, left ++ right)

  // TODO : this is too inefficient
  override def ++(that: IntSet): IntSet =
    (left ++ right ++ that) + element

  override def toString: String = "{" + left + " " + element + " " + right +"}"
}
