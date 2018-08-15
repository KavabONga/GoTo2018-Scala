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
