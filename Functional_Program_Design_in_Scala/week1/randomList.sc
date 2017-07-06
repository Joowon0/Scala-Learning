package week1

object randomList{
  def lists: Generator[List[Int]] =
    for {
      isEmpty <- booleans
      list    <- if (isEmpty) emptyList else nonEmptyList
    } yield list

  def emptyList: Generator[List[Int]] = single(Nil)
  def nonEmptyList: Generator[List[Int]] =
    for { head <- integers
          tail <- lists
    } yield (head :: tail)
}