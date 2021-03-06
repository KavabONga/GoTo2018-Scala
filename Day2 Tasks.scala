def task1() = {
  val a = List(1, 2, 3, 1, 2, 3)
  println(a.last)
  def getLast[P](l : List[P]): P = {
    l match {
      case x :: Nil => x
      case x :: l => getLast(l)
      case _ => throw new Exception
    }
  }
  println(getLast(a))
}

def task2() = {
  val a = List(1, 2, 3, 1, 2, 3)
  println(a.takeRight(2).head)
  def getPreLast[P](l : List[P]): P = {
    l match {
      case x :: _ :: Nil => x
      case x :: l => getPreLast(l)
      case _ => throw new Exception
    }
  }
  println(getPreLast(a))
}
def task3() = {
  val a = List(1, 2, 3, 1, 2, 3)
  val k = 3
  println(a(k))
  def getKthElement[P](l : List[P], k : Int): P = {
    l match {
      case x :: _ if k == 0 => x
      case _ :: ar => getKthElement(ar, k - 1)
      case _ => throw new Exception
    }
  }
  println(getKthElement(a, k))
}
def task4() = {
  val a = List(1, 2, 3, 1, 2, 3)
  println(a.length)
  def getLength[P](l : List[P]) : Int = {
    l match {
      case Nil => 0
      case _ :: ar => getLength(ar) + 1
      case _ => throw new Exception
    }
  }
  println(getLength(a))
}
def task5() = {
  val a = List(1, 2, 3, 1, 2, 3)
  println(a.reverse)
  def getReversed[P](l : List[P]) : List[P] = {
    l match {
      case Nil => Nil
      case x :: ar => getReversed(ar) ::: List(x)
      case _ => throw new Exception
    }
  }
  println(getReversed(a))
}
def task6() = {
  val a = List(1, 2, 3, 3, 2, 1)
  def isPalindrom[P](l : List[P]) : Boolean = a.take(a.length / 2) == a.takeRight(a.length / 2).reverse
  println(isPalindrom(a))
}
def task7() = {
  val a = List(1, 2, 3, 3, 2, 1, 4, 5, 7)
  def removeNthElements[P](l : List[P], n : Int) : List[P] = (0 to l.length).toList.zip(l).filter(pair => (pair._1 + 1) % n != 0).map(pair => pair._2)
  println(removeNthElements(a, 3))
}
task7()
