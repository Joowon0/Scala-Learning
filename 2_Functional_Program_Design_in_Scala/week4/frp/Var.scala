package week4.frp

/**
  * Created by joowon on 17. 7. 19.
  */
class Var[T](expr: => T) extends Signal[T](expr) {
  def update(expr: => T): Unit = ???
}

object Var {
  def apply[T](expr: => T) = new Var(expr)
}
