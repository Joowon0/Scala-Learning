package week1

object randomGenerator {
  trait Generator[+T] {
    self =>                  // alias for "this" (the outer class itself)
    def generate: T
    def map[S](f: T=>S): Generator[S] = new Generator[S] {
      override def generate: S = f(self.generate)
    } // (this.)generate instead of self.generate would make self recursive function
    def flatMap[S](f: T=>Generator[S]): Generator[S] = new Generator[S] {
      override def generate: S = f(self.generate).generate
    }
  }

  val integers = new Generator[Integer] {
    val rand = new java.util.Random
    override def generate: Integer = rand.nextInt()
  }
  val booleans = for { x <- integers } yield x > 0

  def pairs[T,U](t: Generator[T], u: Generator[U]) =
    for { x <- t
          y <- u
    } yield (x,y)

  // unit function of Generator
  def single[T] (x: T): Generator[T] = new Generator[T] {
    override def generate: T = x
  }

  def choose(lo: Int, hi: Int): Generator[Int] =
    for { x <- integers }
      yield lo + x % (hi - lo)

  def oneOf[T](xs: T*): Generator[T] =
    for { x <- choose (0, xs.length) }
      yield xs(x)
}