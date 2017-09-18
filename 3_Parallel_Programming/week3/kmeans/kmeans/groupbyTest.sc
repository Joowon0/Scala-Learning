val a = List(1,2,4,5)
val b: List[Int] = List(0, 1, 2)
def isEven(x: Int) =
  x % 2 == 0
def threeR(x : Int) =
  x % 3

val x  : Map[Boolean, List[Int]] = a groupBy isEven
val ys : Map[Int, List[Int]] = Map() ++ (b map {(_ -> List())})
val y  : Map[Int, List[Int]]=  (a groupBy threeR)
ys ++ y

