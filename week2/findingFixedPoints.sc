// this is for lecture 2.3 Example: Finding Fixed Points
import math.abs

// example of currying
object findingFixedPoints {
  val toleranace = 0.0001
  def isCloseEnough(x:Double, y:Double) =
    abs(x-y) < toleranace *  x * x // don't know why this should be x^2
  def fixedPoint(f: Double => Double) (firstGuess: Double) = {
    def iterate(guess: Double): Double = {
      //println("guess : " + guess)
      val next = f(guess)
      if (isCloseEnough(guess, next)) next // check btn nth guess and n+1th
      else iterate(next)
    }
    iterate(firstGuess)
  }

  fixedPoint(x => 1+ x/2) (1)
  fixedPoint(x => 1000000000+ x/2) (1)
  fixedPoint(x => 1e-20+ x/2) (1)

  def sqrt(x: Double) =
    fixedPoint(y => (y + x / y) / 2) (1.0)
    // prevent varying too much
    // by varying to the mean of y and x / y
    // why??

  sqrt(4)
  sqrt(2)

  // function that takes function
  // and returns a function
  // that returns an average
  def averageDamp(f: Double => Double) (x: Double) =
    (x + f(x)) / 2

  def sqrt2(x:Double) =
    fixedPoint (averageDamp(y => x / y)) (1.0)

  sqrt2(4)
  sqrt2(2)
}