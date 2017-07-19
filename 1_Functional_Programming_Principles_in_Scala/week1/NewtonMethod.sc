object NewtonScope {
  def abs(x: Double) = if (x < 0) -x else x

  // code originate from lec1.5
  // this is a block (lec 1.6)
  def sqrt(x: Double) = {
    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    def isGoodEnough(guess: Double) =
      abs(guess * guess - x) < x * 0.001

    def improve(guess: Double) =
      (guess + x / guess) / 2

    sqrtIter(1.0)
  }

  sqrt(1)
  sqrt(4)
  sqrt(1e-6)
  sqrt(1e60)

  sqrt(0.001)
  0.03162278245070105 * 0.03162278245070105

  sqrt(0.1e-20)
  3.1633394544890125E-11 * 3.1633394544890125E-11

  sqrt(1.0e20)
  1.0000021484861237E10 * 1.0000021484861237E10

  sqrt(1.0e50)
  1.0000003807575104E25 * 1.0000003807575104E25

}