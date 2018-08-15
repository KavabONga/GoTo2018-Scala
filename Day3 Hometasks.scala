def countSignChange(l : List[Int]) : Int = {
  l.tail.foldLeft((0, l.head > 0))((p : (Int, Boolean), x : Int) => {
    if ((x > 0) ^ p._2) (p._1 + 1, x > 0)
    else (p._1, x > 0)
  })._1
}
println(countSignChange(List(1, -1, -1, 1, 1, -1)))

def countCharChange(s : String) : Int = {
  val vowels = List( 'a', 'e', 'i', 'o', 'u', 'y')
  val consonants = (('a' to 'z').toSet diff vowels.toSet).toList
  def varNum(c : Char) = {
    if (vowels.contains(c)) 0
    else {
      if (consonants.contains(c)) 1
      else -1
    }
  }
  s.tail.foldLeft((0, varNum(s.head.toLower)))((p : (Int, Int), c : Char) => {
    if (varNum(c.toLower) + p._2 == 1) (p._1 + 1, varNum(c.toLower))
    else (p._1, varNum(c.toLower))
  })._1
}
println(countCharChange("This is so sad"))

def binaryDivisors(x : Int, t : Int = 2): List[Int] = {
  if (t > x) List()
  else {
    if (x % t == 0) t :: binaryDivisors(x, t * 2)
    else binaryDivisors(x, t * 2)
  }
}
 println(binaryDivisors(12))
