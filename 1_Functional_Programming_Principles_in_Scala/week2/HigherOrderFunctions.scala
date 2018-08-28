package week2

object HigherOrderFunctions {
  def mapReduce(reduce: (Int, Int) => Int, zero: Int)(map: Int => Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int =
      if ( a > b ) acc
      else loop (a + 1, reduce(map(a), acc))
    loop(a, zero)
  }

  def sum     : (Int => Int) => (Int, Int) => Int = mapReduce((x, y) => x + y, 0)
  def product : (Int => Int) => (Int, Int) => Int = mapReduce((x, y) => x * y, 1)


  def sumofInts      : (Int, Int) => Int = sum (x => x)
  def sumOfCubes     : (Int, Int) => Int = sum (x => x * x * x)
  def sumOfFcatorial : (Int, Int) => Int = sum (x => factorial(x))

  def factorial(n: Int) = product (x => x) (1, n)
}
