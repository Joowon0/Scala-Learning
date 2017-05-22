// this is for lecture 4.6
trait Expr
case class Number(n: Int) extends Expr
case class Var(x: String) extends Expr
case class Sum(e1: Expr, e2: Expr) extends  Expr
case class Prod(e1: Expr, e2: Expr) extends Expr


object expr {
  def eval(x: Expr): Int = x match {
    case Number(n) => n
    case Sum (e1, e2) => eval(e1) + eval(e2)
    case Prod(e1, e2) => eval(e1) * eval(e2)
  }
  def show(x: Expr): String = x match {
    case Number(n) => n.toString
    case Var(x) => x
    case Sum(e1, e2) => show(e1) ++ " + " ++ show(e2)
    case Prod(e1, e2) => {
      val fstExp: String = e1 match { // this make sure that parenthesis is there only when needed
        case Sum(e3, e4) => "(" ++ show(e3) ++ " + " ++ show(e4) ++ ")"
        case _ => show(e1)
      }
      val sndExp: String = e2 match {
        case Sum(e3, e4) => "(" ++ show(e3) ++ " + " ++ show(e4) ++ ")"
        case _ => show(e2)
      }
      fstExp ++ " * " ++ sndExp
    }
  }
}

val a = Sum(Number(2), Number(3))
expr.show (a)
expr.eval(a)

val b = Sum(Prod(Number(2), Var("x")), Var("y"))
expr.show(b)

val c = Prod(Sum(Number(2), Var("x")), Var("y"))
expr.show(c)