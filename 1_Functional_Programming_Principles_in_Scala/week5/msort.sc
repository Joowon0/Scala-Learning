// this is for lecture 5.2 Pairs and Tuples
// this is for lecture 5.3 Implicit Parameter
import math.Ordering // defined >, <, ==, ... for many types

def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
  def n = xs.length / 2

  if (n == 0) xs
  else {
    def merge(xs: List[T], ys: List[T]): List[T] =
      (xs, ys) match {
        case (Nil, ys1) => ys1
        case (xs1, Nil) => xs1
        case (x::xs1, y::ys1) =>
          if (ord.lt(x, y)) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }

    val (fst, snd) = xs splitAt n

    merge(msort(fst), msort(snd)) // ord is passed
  }
}

val nums = List (5,1,34,1,32,6,8,99,63,1,3)
val fruits = List ("peach", "apple", "banna", "watermelon")

msort(nums)(Ordering.Int)
msort(nums) // ordering is type inferenced

msort(fruits)(Ordering.String)
msort(fruits)
