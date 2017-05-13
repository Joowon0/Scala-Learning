package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int =
      if (c==0 || c==r) 1
      else pascal(c-1, r-1) + pascal(c, r-1)

  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean =  {
      def balanceIter(chars: List[Char], parenCount:Int) : Boolean = {
        if (chars.isEmpty) parenCount == 0
        else if (parenCount < 0) false
        else if (chars.head == '(') balanceIter(chars.tail, parenCount+1)
        else if (chars.head == ')') balanceIter(chars.tail, parenCount-1)
        else balanceIter(chars.tail, parenCount)
      }
      balanceIter(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def countIter(moneyIter: Int, coinsIter: List[Int]) : Int = {
        if (moneyIter == 0 || coinsIter.isEmpty) 0
        else if (moneyIter < coinsIter.head) countIter(moneyIter, coinsIter.tail)
        else if (moneyIter == coinsIter.head) 1 + countIter(moneyIter, coinsIter.tail)
        else countIter(moneyIter - coinsIter.head, coinsIter) +
          countIter(moneyIter, coinsIter.tail)
      }
      countIter(money, coins.sorted.reverse)
    }
  }
