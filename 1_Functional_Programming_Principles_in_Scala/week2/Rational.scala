// this is for lecture 2.5 and 2.6
package week2

import Math.abs

class Rational(x: Int, y: Int) {
  require(y != 0, "Denominator must be nonzero")

  // make denom to positive
  val (n, d) = if (y > 0) (x, y)  else (-x, -y)

  def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)
  def g = abs(gcd(n, d))

  def numer = n / g
  def denom = d / g


  def + (that: Rational): Rational = {
    val n = numer * that.denom + that.numer * denom
    val d = denom * that.denom
    new Rational(n, d)
  }

  def unary_-(): Rational = new Rational(-n, d)
  def - (that: Rational): Rational = this + -that

  def * (that: Rational): Rational = {
    val n = numer * that.numer
    val d = denom * that.denom
    new Rational(n, d)
  }

  def / (that: Rational): Rational = {
    val n = numer * that.denom
    val d = denom * that.numer
    new Rational (n, d)
  }

  def < (that: Rational): Boolean =
    numer * that.denom < that.numer * denom
  def max (that: Rational): Rational =
    if (this < that) that else this

  override def toString() =
    numer + "/" + denom
}