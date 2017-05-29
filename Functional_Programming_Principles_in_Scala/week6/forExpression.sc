// this is for lecture 6.2 Combinatorial Search and For-Expression

case class Person(name: String, age: Int)

val persons = List(new Person("Joowon", 23), new Person("Seungmin", 240))
for ( p <- persons if p.age > 100) yield p.name
persons filter (p => p.age > 100) map (p => p.name)


def scalarProduct(xs: List[Double], ys: List[Double]): Double =
  (for { (x, y) <- xs zip ys
  } yield (x * y) ). sum

scalarProduct(List(1,2,3), List(-1, -1, -1))