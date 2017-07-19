import scala.io.Source

import scala.io.Source

object x {
  val in = Source.fromURL("http://lamp.epfl.ch/files/content/sites/lamp/files/teaching/progfun/linuxwords.txt")
  val words = in.getLines.toList filter (words => words forall (ch => ch.isLetter))

  val mnem = Map(
    '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
    '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")

  /** Invert the mnem map to give a map from chars 'A' ... 'Z' to '2' ... '9' */
  val charCode: Map[Char, Char] =
    for {(num, alphas) <- mnem
         alpha <- alphas}
         yield (alpha -> num)

  /** Maps a words to the digit string it can represent, e.g. "Java" -> "5282" */
  def wordCode(word: String): String = word.toUpperCase map (charCode)
  wordCode("JAVA")
  wordCode("Java")

  /**
    * A map from digit strings to the words that represent them,
    * e.g. "5285" -> List("Java", "Kata", "Lava", ...)
    * Note: A missing number should map to the empty set, e.g. "1111" -> List()
    */
  val wordsForNum: Map[String, Seq[String]] =
    (words groupBy wordCode) withDefaultValue Seq()

  wordsForNum("1111")

  /** Return all ways to encode a number as a list of words */
  def encode(number: String): Set[List[String]] = {
    def encodeRecur(number: String): Set[List[String]] = {
      if (number.isEmpty) Set(List())
      else {
        for {matchLen   <- 1 to number.length
             matchWords <- wordsForNum (number take matchLen)
             leftWords  <- encode (number drop matchLen)}
          yield matchWords :: leftWords
      }.toSet
    }
    encodeRecur(number)
  }

  encode("7225247386")
}