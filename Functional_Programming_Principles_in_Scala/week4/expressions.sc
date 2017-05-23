// this is for lecture 4.6
trait Expr {
  def eval: Int = this match {
    case Number(n) => n
    case Sum (e1, e2) => e1.eval + e2.eval
    case Prod(e1, e2) => e1.eval * e2.eval
  }
  def show: String = this match {
    case Number(n) => n.toString
    case Var(x) => x
    case Sum(e1, e2) => e1.show ++ " + " ++ e2.show
    case Prod(e1, e2) => {
      val fstExp: String = e1 match { // this make sure that parenthesis is there only when needed
        case Sum(e3, e4) => "(" ++ e3.show ++ " + " ++ e4.show ++ ")"
        case _ => e1 show
      }
      val sndExp: String = e2 match {
        case Sum(e3, e4) => "(" ++ e3.show ++ " + " ++ e4.show ++ ")"
        case _ => e2 show
      }
      fstExp ++ " * " ++ sndExp
    }
  }
}
// case class makes companion objects by default
// Number(3) is now equal to new Number(3)
case class Number(n: Int) extends Expr
case class Var(x: String) extends Expr
case class Sum(e1: Expr, e2: Expr) extends  Expr
case class Prod(e1: Expr, e2: Expr) extends Expr

val a = Sum(Number(2), Number(3))
a.show
a.eval

val b = Sum(Prod(Number(2), Var("x")), Var("y"))
b.show

val c = Prod(Sum(Number(2), Var("x")), Var("y"))
c.show
