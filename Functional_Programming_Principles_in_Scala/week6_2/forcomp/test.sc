//package forcomp

object Anagrams {
  val a = List("123", "432")
  val b:String = a.flatten.toString

  type Word = String
  type Sentence = List[Word]
  type Occurrences = List[(Char, Int)]
  //val dictionary: List[Word] = loadDictionary

  def wordOccurrences(w: Word): Occurrences = {
    val mapping:Map[Char, String] = w groupBy (x => x)

    {for {(letter, ls) <- mapping}
      yield (letter, ls.length)}.toList.sorted
  }

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = s flatMap wordOccurrences

  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = Map()
  //  (dictionary groupBy wordOccurrences) withDefaultValue List()

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = {
    val wordOcc = wordOccurrences(word)
    dictionaryByOccurrences (wordOcc)
  }
  def combinations(occurrences: Occurrences): List[Occurrences] =
    occurrences match {
      case List()              => List(List())
      case (char1, int1) :: li => {
        val tailCombis: List[Occurrences] = combinations(li)
        val newheads =
          for (c <- 1 to int1) yield (char1, int1)

        val newCombis:List[Occurrences] = {
          {for {tailCombi <- tailCombis    // why these two isn't changable??
                intCount  <- (1 to int1)}  // Error : type mismatch
            yield (char1, intCount) :: tailCombi}
        }
        newCombis ++ tailCombis
      }
    }
  combinations(List(('a', 2), ('b', 2)))
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    val xMap = x.toMap
    val yMap = y.toMap
    lazy val zMap = (xMap foldLeft yMap) (subMap)

    def subMap (baseM: Map[Char,Int], subM: (Char,Int)): Map[Char,Int] = {
      val (subChar, subNum) = subM
      val baseNum = baseM(subChar)

      if (baseNum <= subNum)
        baseM - subChar
      else
        baseM updated (subChar, baseNum - subNum)
    }
    zMap.toList.sorted
  }

  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    val occurrences = sentenceOccurrences(sentence)
    def makeCombiRec(occur: Occurrences): List[Sentence] = {
      if (occur.isEmpty)
        List()
      else {
        val headCombi: List[Occurrences] = combinations(occur)
        for {
          headOccur:Occurrences  <- headCombi : List[Occurrences]
          tailOccur:Occurrences  = subtract(occur, headOccur) : Occurrences
          restSents:Sentence     <- makeCombiRec(tailOccur) :List[Sentence]
          headWord:Word          <- dictionaryByOccurrences (headOccur) :List[Word]
        } yield headWord :: restSents
      }
    }

    makeCombiRec(occurrences)
  }
}
