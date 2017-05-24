// this is fer lecture 5.1 More functions on List
// xs.length
// xs.last
def last[T](xs: List[T]): T = xs match {
  case Nil     => throw new Error("no Elements in list")
  case List(x) => x
  case y::ys   => last(ys)
}
// xs.init - all except the last one
def init[T](xs:List[T]): List[T] = xs match {
  case Nil => throw new Error("list is empty")
  case List(x) => Nil
  case y::ys   => y::init(xs)
}
// xs take n
// xs drop n
// xs(n)
// xs ++ ys
def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
  case Nil   => ys
  case z::zs => z :: concat(zs, ys)
}

// xs.reverse
def reverse[T](xs: List[T]): List[T] = xs match {
  case List() => xs
  case y::ys  => reverse(ys) ++ List(y)
}
// xs updated (n, x)
// xs indexOf x
// xs contains x

def removeAt[T](xs: List[T], n: Int) =
  (xs take n) ++ (xs drop n+1)

def a = List(5,3,1,3,65,7,3,2,3,1)
removeAt(a, 2)

def flatten(xs: List[Any]): List[Any] = xs match {
  case Nil   => Nil
  case y::ys => y match {
    case Nil   => flatten(ys)
    case z::zs => flatten(z::zs) ++ flatten(ys)
    case z     => z :: flatten(ys)
  }
}

flatten(List(List(1,1), 2, List(3,List(5,8))))
