def duplicateN[P](n:Int, l : List[P]) = l.flatMap(x => List.fill(n)(x))
println(duplicateN(4, List(1, 2, 3)))
def myFlatten(l : List[Any]): List[Any] = {
  l.flatMap(
    x => x match {
      case ar : List[_] => ar
      case _ => List(x)
    }
  )
}
println(myFlatten(List(List(1, 2), 3)))
def mySlice[P](l : List[P], start : Int, until : Int) = l.take(until).takeRight(until - start)
println(mySlice(List(1, 2, 3, 4, 5, 6), 2, 4))
def rotate[P](k : Int, l : List[P]) = {
  val offset = (k % l.length + l.length) % l.length
  l.takeRight(l.length - offset) ::: l.take(offset)
}
println(rotate(3, List(1, 2, 3, 4, 5, 6, 7, 8)))
