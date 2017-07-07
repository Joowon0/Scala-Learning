package week2

class waterPouringProb(capacity: Vector[Int]){

  // States
  type State = Vector[Int]
  val initialSate = capacity map (x => 0)

  // Moves
  trait Move {
    def change(state: State): State = {
      case Empty(glass) => state updated (glass, 0)
      case Fill (glass) => state updated (glass, capacity (glass))
      case Pour (from, to) => {
        val amount  = state(from) min (capacity(to) - state(to))
        state updated (from, state(from) - amount) updated (to, state(to) + amount)
      }
    }
  }
  case class Empty(glass: Int) extends Move
  case class Fill (glass: Int) extends Move
  case class Pour (from: Int, to: Int) extends Move

  val glassNum = 0 until capacity.length
  // a list of all possible moves
  val moves =
    (for (g <- glassNum) yield Empty(g)) ++
    (for (g <- glassNum) yield Fill(g)) ++
    (for {from <- glassNum
           to <- glassNum
            if from != to} yield Pour(from, to))

  // Paths
  class Path(history: List[Move]) { // history is in reverse
    //def endState: State = (history foldRight initialSate) (_ change _)
    def endState: State = trackState(history)
    private def trackState(xs: List[Move]): State = xs match {
      case Nil => initialSate
      case move :: xs1 => move change trackState(xs)
    }

    def extend(move: Move) = new Path(move :: history)
    override def toString: String = (history.reverse mkString " ") + "--> " + endState
  }

  val initialPath = new Path(Nil)

  def from(paths: Set[Path]): Stream[Set[Path]] =
    if (paths.isEmpty) Stream.empty
    else {
      val more = for {
        path <- paths
        next <- moves map path.extend
      } yield next
      paths #:: from(more)
    }

  val pathSets = from(Set(initialPath))

  def solution(target: Int): Stream[Path] =
    for {
      pathSet <- pathSets
      path    <- pathSet
      if path.endState contains target
    } yield  path
}