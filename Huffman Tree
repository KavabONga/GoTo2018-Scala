sealed abstract class HuffmanTree
case class EmptyNode() extends HuffmanTree {
  override def toString: String = "."
}
case class HuffmanNode(left : HuffmanTree = EmptyNode(),
                       right : HuffmanTree = EmptyNode(),
                       w : Int = 0,
                       id : Int = -1) extends HuffmanTree {
  require(!(left == EmptyNode() ^ right == EmptyNode()))
  private def getWeight(node : HuffmanTree): Int = {
    node match {
      case EmptyNode() => 0
      case h : HuffmanNode => h.weight
    }
  }
  def isLeaf() : Boolean = left == EmptyNode() && right == EmptyNode()
  override def toString: String = s"${weight}(${left.toString}, ${right.toString})"
  val weight = w + getWeight(left) + getWeight(right)
}
def HuffmanUnion(l : List[HuffmanNode]) : HuffmanNode = {
  if (l.length == 1) l.head
  else {
    val sortL = l.sortBy(t => t.weight)
    val a = sortL.head
    val b = sortL(1)
    HuffmanUnion(HuffmanNode(a, b) :: sortL.drop(2))
  }
}
def getEncodings(tree : HuffmanTree) : List[(Int, String)] = {
  tree match {
    case EmptyNode() => throw new Exception
    case node : HuffmanNode =>
      if (node.isLeaf()) List((node.id, ""))
      else {
        val l = getEncodings(node.left)
        val r = getEncodings(node.right)
        l.map(p => (p._1, "0" + p._2)) ::: r.map(p => (p._1, "1" + p._2))
      }
  }
}
def HuffmanEncodings(l : List[Int]) = {
  val nodes = l.zipWithIndex.map(p => HuffmanNode(id = p._2, w = p._1))
  val tree = HuffmanUnion(nodes)
  val encs = getEncodings(tree)
  encs.sortBy(p => p._1).map(p => p._2)
}
def HuffmanString(s : String): String = {
  val chars = s.distinct.toList
  val counts = chars.map(c => s.count(c == _))
  val encs = HuffmanEncodings(counts)
  val enc_dict = chars.zip(encs).toMap
  println(enc_dict)
  s.flatMap(c => enc_dict.getOrElse(c, ""))
}
