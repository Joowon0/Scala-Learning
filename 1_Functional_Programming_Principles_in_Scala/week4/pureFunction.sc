//this is for lecture 4.2 Functions as Objects
trait Function[A, B] {
  def apply(x: A): B
}

// this is same as (x: Int) => x * x
{class AnonFun extends Function1[Int, Int] {
    def apply(x: Int) = x * x
  }
  new AnonFun
}

// this is same as (x: Int) => x * x
new Function1[Int, Int] {
  def apply(x:Int) = x * x
}

// if function names are used, it is converted to objects
val f = new Function1[Int, Int] {
  // def is not a function value
  def apply(x:Int) = x * x
}
f.apply(7)

//converting a function to annonymous one is called eta-expansion