import streams._

val test1 = Bloxorz.Level1
//val allStates = test1.pathsFromStart.take(10).toList.mkString("\n")
val fstState = test1.pathsFromStart.head
fstState._1.neighbors
fstState._1.legalNeighbors

test1.solution

//val neigh = test1.neighborsWithHistory(fstState._1, fstState._2)

def printTF(bool:Boolean): String =
  if (bool) "T"
  else "F"

val x =
{for {
    row <- (0 to 5)
   col:String =
    {for {col <- (0 to 9)}
      yield test1.terrain(test1.Pos(row, col))} map printTF mkString(" ")

  } yield col} mkString "\n"