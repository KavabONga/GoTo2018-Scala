class Rational(m : Int, n : Int = 1) {
  override def toString: String = m.toString + {if (n != 1) s"/$n" else ""}
  private def gcd(x : Int, y : Int):Int = {
    if (x == 0) y
    else {
      if (y == 0) x
      else gcd(y, x % y)
    }
  }
  private def binPow(x : Int, n : Int): Int = {
    if (n == 0) 1
    else {
      if (n % 2 == 0) {
        val t = binPow(x, n / 2)
        t * t
      }
      else binPow(x, n - 1) * x
    }
  }
  val numer : Int = (m * n / n.abs) / gcd(n, m)
  val denom : Int = n.abs / gcd(n, m)
  def + (that : Rational) = new Rational(numer * that.denom + that.numer * denom, denom * that.denom)
  def - (that : Rational) = new Rational(numer * that.denom + that.numer * denom, denom * that.denom)
  def * (that : Rational) = new Rational(numer * that.numer, denom * that.denom)
  def / (that : Rational) = new Rational(numer * that.denom, that.numer * denom)
  def pow(p : Int) : Rational = new Rational(binPow(numer, p), binPow(denom, p))

}
implicit def intToRational(x : Int): Rational = new Rational(x)

val Codes: Map[Char, String] = Map(
  'a' -> ".-", 'b' -> "-...", 'c' -> "-.-.", 'd' -> "-..",
  'e' -> ".", 'f' -> "..-.", 'g' -> "--.", 'h' -> "....",
  'i' -> "..", 'j' -> ".---", 'k' -> "-.-", 'l' -> ".-..",
  'm' -> "--", 'n' -> "-.", 'o' -> "---", 'p' -> ".--.",
  'q' -> "--.-", 'r' -> ".-.", 's' -> "...", 't' -> "-",
  'u' -> "..-", 'v' -> "...-", 'w' -> ".--", 'x' -> "-..-",
  'y' -> "-.--", 'z' -> "--..", '1' -> ".----", '2' -> "..---",
  '3' -> "...--", '4' -> "....-", '5' -> ".....", '6' -> "-....",
  '7' -> "--...", '8' -> "---..", '9' -> "----.", '0' -> "-----",
  ' ' -> "/")
def MorseEncode(s : String): String = s.toList.map(Codes.getOrElse(_, "")).filterNot(_ == "").mkString("|")
def MorseDecode(code : String) : String = {
  val revCodes = Codes.toList.map(p => (p._2, p._1)).toMap
  code.split("\\|").map(c => revCodes.getOrElse(c, "")).mkString("")
}
println(MorseDecode(MorseEncode("this is epic")))

def isSorted(l : List[Int]) = 
  l.tail.foldLeft((true, l.head))((p, x) => (p._1 && x >= p._2, x))._1
println(isSorted(List(3, 3, 5, 6)))

def listContains[T](l : List[T], elem: T) = l.contains(elem)
println(listContains(List('a', 'b', 'c'), 'c'))

def insertSort(l : List[Int]) = 
  l.tail.foldLeft(List(l.head))((l, x) => {
    val p = l.span(_ <= x)
    (p._1 :+ x) ::: p._2
  })
println(insertSort(List(4, 2, 1, 4, 5)))

def factorial(n : Int):Int = if (n == 0) 1 else n * factorial(n - 1)

def placementsRepeated(n : Int) = factorial(n)
def placementsNotRepeated[T](l : List[T]) =
  placementsRepeated(l.length) / l.distinct.map(x => factorial(l.count(x == _))).product
println(placementsNotRepeated(List(1, 1, 3, 3)))

def samplesRepeated(n : Int, k: Int) =
  factorial(n) / factorial(n - k)
def samplesNotRepeated(n : Int, k : Int) =
  factorial(n) / (factorial(k) * factorial(n - k))
