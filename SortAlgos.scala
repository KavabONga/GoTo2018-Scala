def bubbleSort(l : List[Int]) : List[Int] = {
  if (l.isEmpty) List()
  else {
    val newL = l.tail.foldLeft(List(l.head))((ar : List[Int], x) => {
      if (ar.last > x) ar.init :+ x :+ ar.last
      else ar :+ x
    })
    bubbleSort(newL.init) :+ newL.last
  }
}
println(bubbleSort(List( 5, 4, 12, 1, 3)))

def quickSort(l : List[Int]) : List[Int] = {
  if (l.length <= 1) l
  else {
    val a = l.filter(_ <= l.head)
    val b = l.filterNot(_ <= l.head)
    quickSort(a.reverse) ::: quickSort(b)
  }
}
println(quickSort(List(5, 3, 12, 1, 4)))
