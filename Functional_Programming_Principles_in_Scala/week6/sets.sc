// this is for lecture 6.3 Combinatorial Search Example

val fruit = Set("banana", "peach", "grape")
val s = (1 to 6).toSet

s map (_ + 2)
s filter (_ % 2 == 0)
s.isEmpty

s map ( _ / 2 )
s contains 5
