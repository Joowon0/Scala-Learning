// this is for lecture 5.5 Reductions of Lists

abstract class List[T] {
  def reduceLeft(op: (T, T) => T): T = this match {
    case Nil   => throw new Error("Nil,reduceLeft")
    case x::xs => (xs foldLeft x)(op)
  }
  def foldLeft[U](z:U)(op: (U, T) => U): U = this match {
    case Nil   => z
    case x::xs => (xs foldLeft op(z, x))(op)
  }

  def reduceRight(op: (T, T) => T): T = this match {
    case Nil    => throw new Error("Nil.reduceRight")
    case x::Nil => x
    case x::xs  => op(x, xs reduceRight op)
  }
  def foldRight[U](z: U)(op: (T, U) => U): U = this match {
    case Nil   => z
    case x::xs => op(x, (xs foldRight z) (op))
  }
}

case object Nil extends List[Nothing]
case class ::[T](x: T, xs : List[T]) extends List[T]