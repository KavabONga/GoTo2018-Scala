def grayEncode(n : Int): Int = {
  n ^ (n >> 1)
}
def grayDecode(g : Int) : Int = {
  if (g == 0) 0
  else g ^ grayDecode(g >> 1)
}
println(grayDecode(2))
