package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf (
    const(empty),
    for {
      k <- arbitrary[Int]
      m <- oneOf(const(empty), genHeap)
    } yield insert(k, m)
  )
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  /*
    If you insert an element into an empty heap,
    then find the minimum of the resulting heap,
    you get the element back
   */
  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  /*
    If you insert any two elements into an empty heap,
    finding the minimum of the resulting heap should get
    the smallest of the two elements back.
  */
  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    val min = if (a < b) a else b
    findMin(h) == min
  }


  /*
    If you insert an element into an empty heap,
    then delete the minimum,
    the resulting heap should be empty.
   */
  property("del1") = forAll { a: Int =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  /*
  Given any heap, you should get a sorted sequence of elements
  when continually finding and deleting minima.
  (Hint: recursion and helper functions are your friends.)
   */
  property("del2") = forAll { (h: H) =>
    def getList(h1: H): List[Int] =
      if (isEmpty(h1)) List()
      else findMin(h1) :: getList(deleteMin(h1))
    def checkSorted(l: List[Int]): Boolean =
      if (l.isEmpty) true
      else (l.head == l.min) && checkSorted(l.tail)
    checkSorted(getList(h))
  }

  /*
  Finding a minimum of the melding of any two heaps
  should return a minimum of one or the other.
   */
  property("min3") = forAll { (h1: H, h2: H) =>
    val h3 = meld(h1, h2)
    if (isEmpty(h3)) true
    else if (isEmpty(h1)) findMin(h3) == findMin(h2)
    else if (isEmpty(h2)) findMin(h3) == findMin(h1)
    else {
      val min = findMin(h3)
      (min == findMin(h1)) || (min == findMin(h2))
    }
  }
  /*
  If you meld two heaps together,
  the combined heap should have all the items of the two original heaps.
   */
  property("gen2") = forAll { (h1: H, h2: H) =>
    val h3 = meld(h1, h2)

    def checkHeap(h3: H, h1: H, h2: H): Boolean = {
      if (isEmpty(h3)) isEmpty(h1) && isEmpty(h2)
      else {
        val min = findMin(h3)
        if (isEmpty(h1) && isEmpty(h2)) false
        else if (isEmpty(h1)) (min == findMin(h2)) && checkHeap(deleteMin(h3), empty, deleteMin(h2))
        else if (isEmpty(h2)) (min == findMin(h1)) && checkHeap(deleteMin(h3), deleteMin(h1), empty)
        else if (min == findMin(h1)) checkHeap(deleteMin(h3), deleteMin(h1), empty)
        else if (min == findMin(h2)) checkHeap(deleteMin(h3), empty, deleteMin(h2))
        else false
      }
    }
    checkHeap(h3, h1, h2)
  }
}
