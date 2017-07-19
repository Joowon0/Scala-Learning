// for lecture 6.2

object primePairs {
  def isPrime(n: Int):Boolean =
    (2 until n) forall (x => (n % x != 0))
  def primeMap(n: Int): IndexedSeq[(Int, Int)] =
    (1 until n) flatMap (x => (x until n)
      map (y => (x, y))) filter {case(x,y) => isPrime(x+y)}
  def primeFor(n:Int): IndexedSeq[(Int,Int)] =
    for {x <- 1 until n
         y <- x until n
         if isPrime(x+y)}
      yield (x,y)

  primeMap(5)
  primeFor(5)
}