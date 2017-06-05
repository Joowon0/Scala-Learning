// this is for lecture 6.4 Maps
object polynominals {
  class Poly(val terms0: Map[Int, Double]) {
    def this(bindings: (Int,Double)*) = this(bindings.toMap)
    val terms = terms0 withDefaultValue 0.0

    // definition of +
    def +(other: Poly) = new Poly(terms0 ++ (other.terms map adjust))
    def adjust(term: (Int, Double)): (Int, Double) = {
      val (exp, coeff) = term
      exp -> (coeff + terms(exp))
      /*
      // this can be replaced by the defualt value
      terms0 get exp match {
        case Some(coeff1) => exp -> (coeff + coeff1)
        case None         => exp -> coeff
      }*/
    }

    // another definition of +
    def ++(other: Poly) =
      new Poly((other.terms foldLeft terms) (addTerm))
    def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
      val (exp, coeff) = term
      terms + (exp -> (coeff + terms(exp)))
    }

    // toString
    override def toString =
      (for ((exp, coeff) <- terms0.toList.sorted.reverse)
        yield coeff + " x^" + exp) mkString " + "
  }

  val p1 = new Poly(Map(1 -> 2.0, 3 -> 4.0, 5 -> 6.2))
  val p2 = new Poly(Map(0 -> 3.0, 3 -> 7.0))
  val p3 = p1 + p2
  val p5 = p1 ++ p2

  p1.terms(7)

  val p4 = new Poly(1 -> 2.0, 3 -> 4.0, 5 -> 6.2)
}