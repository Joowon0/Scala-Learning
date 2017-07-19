package week1

object randomTree {
  trait Tree
  case class Inner(left: Tree, right: Tree) extends Tree
  case class Leaf (x: Int) extends Tree

  def treeGen: Generator[Tree] =
    for { isLeeaf <- booleans
          tree    <- if (isLeeaf) innerGen else leafGen
    } yield tree

  def innerGen: Generator[Tree] =
    for { lbranch <- treeGen
          rbranch <- treeGen
    } yield Inner(lbranch, rbranch)
  def leafGen: Generator[Tree] =
    for { x <- integers
    } yield Leaf(x)

  treeGen.generate
}