// this is for lecture 2.5 and 2.6
object rationals {
  class Rational(x: Int, y: Int) {
    // test when we initialized
    // if fails, an illegal argument exception
    require(y != 0, "denominator must be nonzero")

    // constructor with an argument
    def this(x:Int) = this(x,1)

    private def gcd(a: Int, b:Int): Int =
      if (b == 0) a else gcd(b, a % b)


    // defining into val is better for often calls
    val numer = x    // calculated immediately
    val denom = y

    // this should only take one rational number
    def add(that: Rational) =
      new Rational (
        numer * that.denom + that.numer * denom,
        denom * that.denom)

    override def toString =  {
      val g = gcd(x,y)
      numer/g + "/" + denom/g // this can end up in overflows
    }

    def neg = new Rational (-numer, denom)
    def sub(that:Rational) = add(that.neg)

    // if ths is less than that
    def less(that:Rational) =
      this.numer * that.denom < this.denom * that.numer
    def max(that:Rational) =
      if (less(that)) that else this
  }

  val x = new Rational(1,3)
  val y = new Rational(5,7)
  val z = new Rational(3,2)

  x.add(y)

  x.neg
  x.sub(y).sub(z)
  y.add(y)

  x.less (y)
  x.max(y)
  y.max(x)

  //val strange = new Rational(1,0)

  new Rational(2)
}


