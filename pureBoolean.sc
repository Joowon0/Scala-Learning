// This is for lecture 4.1 Objects Everywhere
abstract class Boolean {
  def ifThenElse[T](t: => T, e: => T) : T

  def && (x: => Boolean): Boolean = ifThenElse(x, False)
  def || (x: => Boolean): Boolean = ifThenElse(True, x)
  def unary_! : Boolean = ifThenElse(False, True)

  def == (x: => Boolean): Boolean = ifThenElse (x, !x)
  def != (x: => Boolean): Boolean = ifThenElse(!x, x)

  def < (x: => Boolean): Boolean = ifThenElse(False, x)
  def > (x: => Boolean): Boolean = ! (this < x)
  def <= (x: => Boolean): Boolean = this < x || this == x
  def >= (x: => Boolean): Boolean = this > x || this == x
}

object True extends Boolean {
  def ifThenElse[T](t: => T, e: => T) : T = t
}

object False extends Boolean {
  def ifThenElse[T](t: => T, e: => T) : T = e
}

False < True
True < False
