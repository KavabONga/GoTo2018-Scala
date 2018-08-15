def countSignChange(l : List[Int]) : Int = {
  l.tail.foldLeft((0, l.head > 0))((p : (Int, Boolean), x : Int) => {
    if ((x > 0) ^ p._2) (p._1 + 1, x > 0)
    else (p._1, x > 0)
  })._1
}
println(countSignChange(List(1, -1, -1, 1, 1, -1)))
