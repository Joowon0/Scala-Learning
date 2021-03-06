package week2

class waterPouringProb(capacity: Vector[Int]) {

// States
  type State = Vector[Int]
  val initialState = capacity map (x => 0)

// Moves
  trait Move {
    def change(state: State): State = this match {
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
          to   <- glassNum
          if from != to} yield Pour(from, to))

// Paths
  class Path(history: List[Move], val endState: State) { // history is in reverse
    //def endState: State = (history foldRight initialState) (_ change _)
    def extend(move: Move) = new Path(move :: history, move change endState)
    override def toString: String = (history.reverse mkString " ") + "--> " + endState
  }
  val initialPath = new Path(Nil, initialState)

  // generate all paths
  def from(paths: Set[Path], explored: Set[State]): Stream[Set[Path]] =
    if (paths.isEmpty) Stream.empty
    else {
      val more = for {
        path <- paths
        next <- moves map path.extend
        if !(explored contains next.endState)
      } yield next
      paths #:: from(more, explored ++ (more map (_.endState)))
    }
  val pathSets = from(Set(initialPath), Set(initialState))

  // pick out all solutions from pathSets
  def solution(target: Int): Stream[Path] =
    for {
      pathSet <- pathSets
      path    <- pathSet
      if path.endState contains target
    } yield path
}
