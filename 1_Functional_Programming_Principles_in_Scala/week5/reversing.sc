// this is for lecture 5.5 Reduction of Lists
def sum(xs: List[Int])     = (xs foldLeft 0) (_ + _)
def product(xs: List[Int]) = (xs foldLeft 1) (_ * _)

sum(List(1,2,3,4))
product(List(1,2,3,4))

def reverse[T](xs: List[T]): List[T] =
  (xs foldLeft List[T]()) ((xs1,x) => x::xs1)

reverse(List(1,2,3,4))


def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]())( f(_)::_ )
def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0)( (_,xs) => xs+1 )

mapFun(List(1,2,3), (x: Int) => x*x)
lengthFun(List(1,2,3))