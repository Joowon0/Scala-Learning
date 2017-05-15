object test {
  type Set = Int => Boolean
  def contains(s: Set, elem: Int): Boolean = s(elem)
  def singletonSet(elem: Int): Set = {
    x:Int => x == elem
  }
  def union(s: Set, t: Set): Set = {
    x:Int => s(x) || t(x)
  }
  def intersect(s: Set, t: Set): Set = {
    x:Int => s(x) && t(x)
  }
  def diff(s: Set, t: Set): Set = {
    x:Int => s(x) && !t(x)
  }
  def filter(s: Set, p: Int => Boolean): Set = {
    x:Int => s(x) && p(x)
  }

  val bound = 1000
  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (contains(s,a) && !p(a)) false
      else if (a == bound) true
      else iter(a+1)
    }
    iter(-bound)
  }
  def exists(s: Set, p: Int => Boolean): Boolean = {
    !forall(s,x=>(!p(x)))
  }
  def a = singletonSet(1)
  def b = singletonSet(2)
  def c = singletonSet(3)
  def d = singletonSet(4)
  def e = union(union(union(a,b),c),d)
  toString(e)
  forall(c,isEven)
  exists(c,isEven)

  toString(map(b, x=>x+1))
  toString(map(e,doubling))
  forall(map(e,doubling),isEven)
  exists(map(e,doubling),isSeven)
  exists(union(map(e,doubling), singletonSet(7)),isSeven)


  def isEven (x:Int): Boolean = x%2==0
  def isSeven(x:Int): Boolean = x==7
  def doubling (x:Int) = 2 * x

  /**
    * Returns a set transformed by applying `f` to each element of `s`.
    */
  def map(s: Set, f: Int => Int): Set = {
    x=> exists(s, y=>contains(s,y)&&f(y)==x)
  }


  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }
  def printSet(s: Set) {
    //println(toString(s))
  }


}