/**
  * Exercise 1
  */
def pascal(c: Int, r: Int): Int =
  if (c==0 || r==c) 1
  else pascal(c-1, r-1) + pascal(c, r-1)

pascal(0,2)
pascal(1,2)

pascal(1,3)
pascal(2,2)




/**
  * Exercise 2
  */
def balance(chars: List[Char]): Boolean = {
  def balanceIter(chars: List[Char], parenCount:Int) : Boolean = {
    if (chars.isEmpty) parenCount == 0
    else if (parenCount < 0) false
    else if (chars.head == '(') balanceIter(chars.tail, parenCount+1)
    else if (chars.head == ')') balanceIter(chars.tail, parenCount-1)
    else balanceIter(chars.tail, parenCount)
  }
  balanceIter(chars, 0)
}

balance("(if (zero? x) max (/ 1 x))".toList)
balance("I told him (that it’s not (yet) done). (But he wasn’t listening)".toList)

balance(":-)".toList)
balance("())(".toList)


/**
  * Exercise 3
  */
def countChange(money: Int, coins: List[Int]): Int = {
  def checkIf (coins: List[Int], coinsIter :List[Int], count : Int) : Boolean = {
    if (coins.isEmpty) count == money
    else checkIf(coins.tail, coinsIter.tail, count + coins.head * coinsIter.head)
  }
  def countIter(moneyIter: Int, coinsIter: List[Int]) : Int = {
    if (moneyIter == 0 || coinsIter.isEmpty) 0
    else if (moneyIter < coinsIter.head) countIter(moneyIter, coinsIter.tail)
    else if (moneyIter == coinsIter.head) 1 + countIter(moneyIter, coinsIter.tail)
    else countIter(moneyIter - coinsIter.head, coinsIter) +
      countIter(moneyIter, coinsIter.tail)
  }
  countIter(money, coins.sorted.reverse)
}

countChange(4, List(1,2))
countChange(4, List(1,2,3))

List(2,34,5,7).sorted.reverse
