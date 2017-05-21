def factorial (x: Integer) :Integer = {
  def factInter(x: Integer, acc: Integer) : Integer=
    if (x == 1) acc
    else factInter(x-1, acc*x)

  factInter(x, 1)
}

factorial(3)
factorial(4)
factorial(5)
