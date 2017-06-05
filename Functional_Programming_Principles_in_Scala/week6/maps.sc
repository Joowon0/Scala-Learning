// this is for lecture 6.4 Maps
object maps {
  val romanNumerals = Map("A" -> 1, "B" -> 3, "C" -> 6)
  val capitalOfCountry = Map("US" -> "Washington", "Korea" -> "Seoul")

  capitalOfCountry("Korea")
  //capitalOfCountry("andorra") // makes a NoSuchElementException
  capitalOfCountry get "Korea"
  capitalOfCountry get "andorra"


  // definition of Option trait
  object myOption {
    trait Option[+A] // similar to Haskell Maybe
    case class Some[+A](value: A) extends Option[A]
    object None extends Option[Nothing]
  }

  // show function of capitalOfCountry
  def showCapital(country: String) = capitalOfCountry.get(country) match {
    case Some(capital) => capital
    case None => "data is missing"
  }

  showCapital("Korea")
  showCapital("andorra")


  // set default value
  val cap1 = capitalOfCountry withDefaultValue ("<unknown>")
  cap1("Korea")
  cap1("andorra")
}

