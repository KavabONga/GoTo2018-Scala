def grayCode(n : Int): List[String] = {
  if (n == 0) List("")
  else {
    val c = grayCode(n - 1)
    c.map("0" + _) ::: c.reverse.map("1" + _)
  }
}
println(grayCode(4).mkString("\n"))
