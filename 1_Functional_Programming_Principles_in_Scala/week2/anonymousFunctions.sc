// this is for lecture 2.1 Higher-Order Functions
object anonymousFunctions{
  // sum from a to b
  // tail-recursion
  def sum(f: Int => Int, a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a+1, acc + f(a))
    }
    loop(a, 0)
  }

  def sumInts(a: Int, b: Int)  =
    sum (x => x, a, b)
  def sumCubes(a: Int, b: Int) =
    sum (x => x * x * x, a, b)
  //def sumFacs(a: Int, b: Int)  =
  //  sum (fac, a, b)

  sumInts(1,5)
  sumCubes(4,5)
}
