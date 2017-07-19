// lecture 6.5
object polynimials {
  class Poly(terms0: Map[Int,Double]) {
    def this(bindings: (Int, Double)*) = this(bindings.toMap)
    val terms = terms0 withDefaultValue 0.0

    def + (other: Poly ) = new Poly ((other.terms foldLeft terms) (addTerm))

    def addTerm (terms: Map[Int, Double], term: (Int, Double)) = {
      val (exp, coeff) = term
      terms + (exp -> (coeff + terms(exp)))
    }

    /*
    def + (other: Poly) = new Poly(terms ++ (other.terms map adjust))

    def adjust(term : (Int, Double)): (Int, Double) = {
      val (exp, coeff) = term
      exp -> (coeff + terms(exp))
    }*/

    override def toString =
      (for {(exp, coeff) <- terms.toList.sorted.reverse}
        yield coeff + "x^" + exp) mkString " + "
  }

  val p1 = new Poly(Map(1->3.0, 2->2.0, 3->1.0))
  val p2 = new Poly(Map(0->3.0, 1->2.0, 2->1.0))
  val p3 = p1 + p2

  val p4 = new Poly(0->3.0, 1->2.0, 2->1.0)
}