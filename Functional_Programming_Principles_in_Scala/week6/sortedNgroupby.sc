// this is for lecture 6.4 Maps
object sortedNgroupby {
  val fruit = List("apple", "orange", "pineapple", "peach")
  fruit sortWith(_.length < _.length)
  fruit.sorted // alphabet order

  fruit groupBy (_.head)
  fruit groupBy (_.length)
}