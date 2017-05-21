abstract class Intset {
  def incl(x:Int): Intset
  def contains(x:Int): Boolean
  def union(other: Intset): Intset
}

object Empty extends Intset {
  def incl(x:Int): Intset = {
    new NonEmpty (x, Empty, Empty)
  }
  def contains(x:Int): Boolean = false
  def union(other: Intset): Intset = other

  override def toString() = "."
}

class NonEmpty(elem: Int, left: Intset, right: Intset) extends Intset {
  def incl(x: Int): Intset = {
    if (x < elem) new NonEmpty (elem, left incl x, right)
    else if (x > elem) new NonEmpty (elem, left, right incl x)
    else this
  }
  def contains(x: Int): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true

  def union(other: Intset): Intset = {
    ( (left union right) union other) incl elem
  }
  override def toString() = "{" + left + elem + right + "}"
}

val a = new NonEmpty(3, Empty, Empty)
val b = a.incl(5)
val c = b.incl(7)
val d = c.incl(1)

val x = new NonEmpty(2, Empty, Empty)
val y = x.incl(0)
val z = y.incl(6)
val w = z.incl(4)

d union w