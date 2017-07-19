// this is for lecture 6.2 Combinatorial Search and For-Expression

def isPrime(n: Int) : Boolean = (2 until n) forall (x => n % x != 0)

// generate (i,j)
// 1. 1 <= j < i < n
// 2. i + j is prime

def pairs(n: Int) = // IndexedSeq is superclass of Vector and Range subclass of Seq
  (1 until n) flatMap (i => // same as flat.map
    (1 until i) map (j => (i,j))) filter
      {case (x, y) => isPrime(x+y)}
pairs(10)

def pairs2(n: Int) =
  for {
    i <- 1 until n
    j <- 1 until i
    if isPrime(i+j)
  } yield (i, j)
pairs(10)
