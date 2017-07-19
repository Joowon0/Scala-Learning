package week3

/**
  * Created by joowon on 17. 7. 12.
  */
class loops {
  def power(x: Double, exp: Int): Double = {
    var r = 1.0
    var i = exp
    while (i > 0)
      { r = r * x; i = i -1}
    r
  }

  def WHILE(condition: => Boolean) (command: => Unit): Unit =
    if (condition) {
      command
      WHILE(condition) (command)
    }
    else ()

  def REPEAT(command: => Unit) (condition: => Boolean): Unit = {
    command
    if (condition) ()
    else REPEAT(command) (condition)
  }

  // these two are same
  for (i <- 1 to 3; j <- "abc") println(i + " " + j)
  (1 to 3) foreach (i => "abc" foreach (j => println(i + " " + j)))

  // def foreach(f: T => Unit): Unit
  // apply 'f' to each element of the collection
}
