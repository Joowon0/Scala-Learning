package week1

object scalaTest {
  def test[T](g: Generator[T], numTimes: Int = 100)(test: T => Boolean): Unit = {
    for (i <- 0 until numTimes) {
      val value = g.generate
      assert(test(value), "test failed for " + value)
    }
    println("passed " + numTimes + "tests")
  }

  test(pairs(lists, lists)) {
    case (xs, ys) => (xs ++ ys).length > ys.length
  }
}