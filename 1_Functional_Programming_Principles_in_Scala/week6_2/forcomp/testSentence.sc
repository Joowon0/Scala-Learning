import forcomp.Anagrams._ //{Occurrences, Word, dictionary}

def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
  val occurrences = sentenceOccurrences(sentence)

  val len = (sentence foldLeft "") (_ + _) .length
  // check if length of sentence is smaller
  def checkLen(s: Sentence): Boolean = {
    val sLen = (s foldLeft "") (_ + _) .length
    sLen <= len
  }
  // combine all words
  def combiWords(words: List[Word]): List[Sentence] =
    words match {
      case List()     => List(List())
      case word::tail => {
        val tailWords = combiWords(tail)
        val withWord = for{ t1 <- tailWords } yield word::t1

        (withWord ++ tailWords) filter (sent => checkLen(sent))
      }
    }
  // check if a sentence is anagram
  def ifAnagram(x: Sentence): Boolean = {
    val sOccurr = sentenceOccurrences(sentence)
    val xOccurr = sentenceOccurrences(x)
    lazy val zOccurr = xOccurr zip sOccurr

    (sOccurr.length == xOccurr.length) &&
      (zOccurr forall {case (x1, s1) => x1 == s1})
  }

  // all combinations of the sentence
  val allCombi = combinations(sentenceOccurrences(sentence))
  // all possible words that can be a part of anagram sentence
  val validWords:List[Word] = (allCombi map dictionaryByOccurrences) flatten
  // all combinations that is anagram with sentence
  val matchCombi = combiWords(validWords) filter ifAnagram
  // permutate all possible combinations
  matchCombi flatMap (_.permutations)
}

val s1 = List()
val s2 = List("I", "love", "you")
val s21 = List("love", "I", "you")
val s22 = List("love", "I", "yuu")

val s3 = List("LinuxRulez")
val allCombi = combinations(sentenceOccurrences(s3))
val validWords:List[Word] = (allCombi map dictionaryByOccurrences) flatten
val len = (s3 foldLeft "") (_ + _) .length
sentenceAnagrams(s1)
sentenceAnagrams(s2)
sentenceAnagrams(s3).length

combiWords(validWords).length

def combiWords(words: List[Word]): List[Sentence] =
  words match {
    case List()     => List(List())
    case word::tail => {
      val tailWords = combiWords(tail)
      val withWord = for{ t1 <- tailWords } yield word::t1

      (withWord ++ tailWords) filter (sent => checkLen(sent))
    }
}

def checkLen(s: Sentence): Boolean = {
  val sLen = (s foldLeft "") (_ + _) .length
  sLen < len
}

sentenceOccurrences(s2)
sentenceOccurrences(s21)
sentenceOccurrences(s22)
ifAnagram(s2, s21)
ifAnagram(s2, s22)
def ifAnagram(x: Sentence, y: Sentence): Boolean = {
  val xOccurr = sentenceOccurrences(x)
  val yOccurr = sentenceOccurrences(y)
  val zOccurr = xOccurr zip yOccurr

  zOccurr forall {case (x1, y1) => x1 == y1}
}

validWords.length
//combinations(sentenceOccurrences(s2))
//sentenceAnagrams(s1)
//sentenceAnagrams(s2)