package week4.frp

/**
  * Created by joowon on 17. 7. 19.
  */
class Signal[T](expr: => T) {
  import Signal._
  // current expression
  private var myExpr : () => T = _
  // current value
  private var myValue: T       = _
  private var observers: Set[Signal[_]] = Set()
  // initialize myExpr and myValue
  update(expr)

  protected def update(expr: T): Unit = {
    myExpr = () => expr
    computeValue()
  }
  def computeValue(): Unit = {
    myValue = caller.withValue(this)(myExpr())
  }

  def apply(): T = {
    observers += caller.value
    assert(!caller.value.observers.contains(this), "cyclic signal definition")
    myValue
  }
}

// unimplemented
object NoSignal extends Signal[Nothing](???) { ??? }

object Signal {
  // _ type means it can take any value type
  val caller = new StackableVariable[Signal[_]](NoSignal)
  def apply[T](expr: => T) = new Signal(expr)
}
