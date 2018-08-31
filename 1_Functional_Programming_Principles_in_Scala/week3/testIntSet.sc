import week3.Empty
import week3.NonEmpty

val a = new NonEmpty(Empty,3, Empty)
val b = a + 5
val c = b + 7
val d = c + 1

d contains 5
val e = d - 5
d contains 0
val f = d - 0

val x = new NonEmpty(Empty, 2, Empty)
val y = x + 0
val z = y + 6
val w = z + 5

d ++ w