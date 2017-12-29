package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    def compute(a : Double, b: Double, c: Double): Double = {
      b * b - 4 * a * c
    }
    Var(compute( a(), b(), c()))
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    def compute(a: Double, b: Double, delta: Double) : Set[Double] = {
      if (delta < 0)
        Set()
      else if (delta == 0)
        Set((-b) / 2 * a)
      else {
        val sq = Math.sqrt(delta)
        Set((-b + sq) / 2 * a, (-b - sq) / 2 * a)
      }
    }
    Var(compute( a(), b(), delta()))
  }
}
