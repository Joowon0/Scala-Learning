package week2

object streams {
  def from(n: Int): Stream[Int] = n #:: from(n+1)

  val nats = from(0)
  nats map (_ * 4)

  // a stream of infinite primes
  object Primes {
    def sieve(s: Stream[Int]): Stream[Int] =
      s.head #:: (s.tail filter (_ % s.head != 0))
    val primes = sieve(from(2))

    //primes take 10 toList
  }

  object Sqrt {
    def sqrtStream(x: Double): Stream[Double] = {
      def improve(guess: Double) = (guess + x / guess) / 2
      lazy val guesses: Stream[Double] = 1 #:: (guesses map improve)
      guesses
    }
    def isGoodEnough(guess: Double, x: Double) =
      math.abs(guess * guess - x) < 0.0001 * x

    def findSqrt(n: Int): Stream[Double] =
      sqrtStream(n) filter (isGoodEnough(_, n))

    //findSqrt(4) take (10) toList
  }
}
