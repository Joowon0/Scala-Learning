// this is for lecture 2.2 Currying
object currying {
  def sum(f:Int => Int): (Int, Int) => Int = {
    def sumF(a: Int, b: Int): Int =
      if (a > b) 0
      else f(a) + sumF(a + 1, b)
    sumF
  }

  // this is exactly the same as sum but shorter
  // (Int => Int) => (Int, Int) => Int
  def sum2(f: Int => Int) (a: Int, b: Int): Int =
    if (a > b) 0
    else f(a) + sum(f) (a + 1, b)

  def sumInts = sum (x => x)
  def sumCubes = sum ( x => x * x * x)
  def sumFactorial = sum (fact)

  def fact(x: Int): Int =
    if (x == 1) 1
    else x * fact(x-1)

  sumCubes(1, 10)
  sumFactorial(10, 20)

  // this is fucntion
  sum (fact)
  // this is value
  (sum(fact)) (10, 20)


}