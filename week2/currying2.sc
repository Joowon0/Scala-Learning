// this is for lecture 2.2 Currying
object currying2 {
  def product (f: Int => Int) (a:Int, b: Int) : Int =
    if (a > b) 1
    else f(a) * product(f) (a+1, b)

  def factorial(a: Int) : Int = product (x=>x) ( 1, a)

  factorial(5)


  // a function that resolve both sum and product
  // g - sum or product
  // x - base case (should be identity of g)
  // f - how it will be calculate
  // a, b - interval from a to be
  def interval (g: (Int, Int) => Int, x:Int) (f: Int => Int) (a: Int, b: Int) : Int =
    if (a > b) x
    else g(f(a), interval (g, x) (f) (a+1, b))

  interval ((x, y) => x * y, 1) (x => x) (1,5)
  interval ((x, y) => x + y, 0) (x => x) (1,5)

  // what we did in class
  // same as interval function
  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero:Int) (a: Int, b: Int): Int =
    if (a > b) zero
    else combine(f(a), mapReduce(f, combine, zero)(a+1,b))

  def product2 (f: Int => Int) (a:Int, b: Int) : Int =
    mapReduce(f, (x, y) => x * y, 1) (a, b)

  product2(x=>x)( 1, 5)

}