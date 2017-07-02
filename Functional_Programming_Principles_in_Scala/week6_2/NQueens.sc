// for lecture 6.3

object NQueens {
  def queens(n: Int): Set[List[Int]] = {
    def playQueen(k: Int): Set[List[Int]] = {
      if (k == 0) Set(List())
      else
        for {queens <- playQueen(k - 1)
             col <- 0 until n
             if isSafe(col, queens)} yield col :: queens
    }
    playQueen(n)
  }

  def isSafe(col: Int, queens: List[Int]): Boolean = {
    val row = queens.length
    val queenP = queens zip (row - 1 to 0 by -1)
    queenP forall {
      case (queenCol, queenRow) => queenCol != col &&
                                   math.abs(col - queenCol) != row - queenRow
    }
  }

  def printQueens(n: Int): String = {
    val qs:Set[List[Int]] = queens(n)
    def printQueen(ans: List[Int]) = {
      val lines =
        for (col <- ans.reverse)
          yield Vector.fill(ans.length)("* ").updated(col, "X ").mkString
      "\n" + (lines mkString "\n")
    }
    val printList = qs map (printQueen(_))
    printList mkString "\n"
  }

  queens(4)
  printQueens(4)
  printQueens(5)
}