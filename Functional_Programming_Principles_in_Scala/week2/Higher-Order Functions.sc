// this is for lecture 2.1 Higher-Order Functions
object higherOrderFunctions {
  // sum from a to b
  def sum(f: Int => Int, a: Int, b: Int): Int =
    if (a > b) 0
    else  f(a) + sum(f, a+1, b)

  def sumInts(a: Int, b: Int)  = sum (id, a, b)
  def sumCubes(a: Int, b: Int) = sum (cube, a, b)
  def sumFacs(a: Int, b: Int)  = sum (fac, a, b)

  def id(x: Int) :Int     = x
  def cube(x: Int) : Int  = x * x * x
  def fac (x : Int) : Int = if (x == 1) 1 else (x * fac(x-1))


  sumInts(1,5)
  sumCubes(4,5)
  sumFacs(4,5)
}

