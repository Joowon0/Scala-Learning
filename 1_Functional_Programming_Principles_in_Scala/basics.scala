import scala.annotation.tailrec

object basics {
  @tailrec
  def gcd(a: Int, b: Int): Int =
    if (b == 0) a
    else gcd(b, a % b)

  def factorial (n : Int): Int = {
    @tailrec
    def factIter(a: Int, acc: Int): Int = {
      if (a > n) acc
      else factIter(a+1, acc * a)
    }
    factIter(1, 1)
  }

  def fibonachi(n : Int): Int = {
    @tailrec
    def fibIter(i: Int, a: Int, b: Int): Int = {
      if (i >= n) b
      else fibIter(i+1, b, a+b)
    }
    fibIter( 1, 0, 1)
  }


  def main(args: Array[String]): Unit = {
    println(gcd(14, 21))
    println(factorial(5))

    var i = 1
    while (i < 10) {
      println(fibonachi(i))
      i = i+1
    }
  }
}
