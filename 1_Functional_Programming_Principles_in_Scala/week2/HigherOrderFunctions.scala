// this is for lecture 2.1 Higher-Order Functions
object HigherOrderFunctions {
  def GofF (g: (Int, Int) => Int, base: Int) (f: Int => Int) (a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int =
      if ( a > b ) acc
      else loop (a + 1, g(f(a), acc))
    loop(a, base)
  }

  def sum : (Int => Int) => (Int, Int) => Int = GofF((x, y) => x+y, 0)
  def product : (Int => Int) => (Int, Int) => Int = GofF((x, y) => x*y, 1)

  
  def sumofInts      : (Int, Int) => Int = sum (x => x)
  def sumOfCubes     : (Int, Int) => Int = sum (x => x * x * x)
  def sumOfFcatorial : (Int, Int) => Int = sum (x => factorial(x))

  def factorial(n: Int) = product (x => x) (1, n)
}