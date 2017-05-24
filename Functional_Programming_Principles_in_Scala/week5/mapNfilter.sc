// this is for lecture 5.4

// map
def squareList1(xs: List[Int]): List[Int] =
  xs match {
    case Nil => Nil
    case y :: ys => (y * y) :: squareList1(ys)
  }

def squareList2(xs: List[Int]): List[Int] =
  xs map (x=> x*x)

def a = List (1,2,3)
squareList1(a)
squareList2(a)


// filter
val nums = List (5,1,34,1,32,6,8,99,63,1,3)
val fruits = List ("peach", "apple", "banna", "watermelon")

nums filter (x=> x > 10)
nums filterNot (x=> x > 10)
nums partition (x=> x > 10) // filter ++ filterNot

nums takeWhile (x => x < 10) // take until satisfy
nums dropWhile (x => x < 10)
nums span (x => x < 10) // takeWhile ++ dropWhile


def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 => {
    val (fst, rest) = xs span (y => y == x)
    fst :: pack(rest)
  }
}
def encode[T](xs: List[T]): List[(T, Int)] = {
  pack(xs) map (x => (x.head, x.length))
}

val x = pack(List("a", "a", "a", "b", "c", "c", "a"))
val t = encode(List("a", "a", "a", "b", "c", "c", "a"))