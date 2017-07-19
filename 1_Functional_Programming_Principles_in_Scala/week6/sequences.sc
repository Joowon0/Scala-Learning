// this is for lecture 6.1 Other Collections

// Vector
val nums = Vector(1,2,3,-88)
val people = Vector("Joowon", "Seungmin")

// cannot change but make new one
3 +: nums // :: are for lists
nums :+ 4



//Arrays
val xs : Array[Int] = Array(1,8,3)
xs map (x => 2 * x)


// Strings
val ys: String = "Hello World!"
ys filter (_.isUpper)


// Ranges
val r: Range = 1 until 5 // 1,2,3,4
val s: Range = 1 to 5    // 1,2,3,4,5
1 to 10 by 3             // 1,4,7,10
6 to 1 by -2             // 6,4,2


xs exists (x => x % 2 == 0)
xs forall (x => x % 2 == 0)
val zs = List(4,5,6) zip ys
zs.unzip

def combinations(n:Int, m:Int) = (1 to n) flatMap ( x => (1 to m) map (y=>(x,y)))
combinations(2,7)
ys flatMap (c => List(c, '.'))

xs.sum
xs.product
xs.max
xs.min


def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
  (xs zip ys).map (xy => xy._1 * xy._2).sum
def scalarProduct2(xs: Vector[Double], ys: Vector[Double]): Double =
  (xs zip ys).map {case (x,y) => x * y}.sum

scalarProduct(Vector(1,2,3), Vector(-1, -1, -1))
scalarProduct2(Vector(1,2,3), Vector(-1, -1, -1))


def isPrime(n: Int) : Boolean = (2 until n) forall (x => n % x != 0)
isPrime(3)
isPrime(4)