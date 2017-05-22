// this is for lecture 4.7
def isort(xs: List[Int]): List[Int] = xs match {
  case List() => List()
  case y::ys  => insert(y, isort(ys))
}
def insert (x: Int, xs: List[Int]): List[Int] = xs match {
  case List() => List(x)
  case y::ys  =>  if (x > y) y :: insert(x, ys)  else x::xs
}

isort(List(1,2,3,4))
isort(List(4,3,2,1))
isort(List(4,1,3,2))