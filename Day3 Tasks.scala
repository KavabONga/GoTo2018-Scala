def RLE(s : String) = {
  if (s.isEmpty) List()
  else {
    s.tail.foldLeft(List((1, s.head)))((l, c) => {
      if (l.last._2 == c)
        l.init :+ (l.last._1 + 1, c)
      else
        l :+ (1, c)
    }).map(p => if (p._1 == 1) p._2 else p)
  }
}
print(RLE("Uahhahhaaaa"))

def RLEDecode(l : List[Any]): String = {
  if (l.isEmpty) ""
  else {
    {l.head match {
    case c : Char => c.toString
    case t : (Int, Char) => List.fill(t._1)(t._2).mkString("")
    case _ => throw new Exception
    }} + RLEDecode(l.tail)
  }
}
println(RLEDecode(List('a', (3, 'h'), 'a')))

def isOdd(x : Int) = x % 2 == 1
println(isOdd(3))

def isEven(x : Int) = x % 2 == 0
println(isEven(-4))

def isSquared(x : Int) = {
  def mostSqrt(y : Int, s : Int = 0) : Int = {
    if ((s + 1) * (s + 1) > y) s
    else mostSqrt(y, s + 1)
  }
  val t = mostSqrt(x)
  t * t == x
}
println(isSquared(4))

def getDigits(x : Int, firstCall : Boolean = true) : List[Int] = {
  if (x == 0)
    if (firstCall) List(0)
    else List()
  else
    getDigits(x / 10) :+ x % 10
}

def sumOfDigits(x : Int) = getDigits(x).sum
println(sumOfDigits(12345))

def compositionOfDigits(x : Int) = getDigits(x).product
println(compositionOfDigits(12345))

def getDivisors(x : Int, d : Int = 1): List[Int] = {
  if (d > x) List()
  else {
    if (x % d == 0) d :: getDivisors(x, d + 1)
    else getDivisors(x, d + 1)
  }

}

def nthGreatestDivisor(x : Int, n : Int) =
  getDivisors(x)(n - 1)
println(nthGreatestDivisor(12, 3))

def numOfDivisors(x : Int) = getDivisors(x).length
println(numOfDivisors(100007))

def sumOfDivisors(x : Int) = getDivisors(x).sum
println(sumOfDivisors(100000))

def isPrime(x : Int) = {
  def primeCheck(n : Int, d : Int = 1): Boolean = {
    if (d * d > 1)
      false
    else {
      if (n % d == 0)
        false
      else
        primeCheck(n, d + 1)
    }
  }
  primeCheck(x)
}
println(isPrime(1000000007))
