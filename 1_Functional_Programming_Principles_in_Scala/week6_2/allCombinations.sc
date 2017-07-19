// for lecture 6.1

object allCombinations {
  def combi(m: Int, n: Int): IndexedSeq[(Int, Int)] =
      (1 to m) flatMap (x => (1 to n) map (y => (x, y)))

  combi (1, 1)

  def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
    (xs zip ys).map {case (x,y) => x * y}.sum
  def scalarProduct2(xs: Vector[Double], ys: Vector[Double]): Double =
    (for {(x,y) <- xs zip ys} yield (x * y)).sum
  scalarProduct(Vector(1,2,3), Vector(1,2,3))
  scalarProduct2(Vector(1,2,3), Vector(1,2,3))

  def isPrime(n: Int):Boolean =
    (2 until n) forall (x => (n % x != 0))
  isPrime(8)
  isPrime(17)

}