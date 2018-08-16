sealed abstract class LinkedList[T] {
  def map[B](func: T => B) : LinkedList[B]
  def ::: (l: LinkedList[T]) : LinkedList[T]
  def :+ (v : T) : LinkedList[T]
  override def toString: String
}

case class EmptyList[T]() extends LinkedList[T] {
  def map[B](func: T => B) : LinkedList[B] = EmptyList[B]()
  def ::: (l: LinkedList[T]) : LinkedList[T] = l
  def :+ (v : T) : LinkedList[T] = ListBox[T](v)
  override def toString() = "Empty"
}
case class ListBox[T](value : T,
                      next : LinkedList[T] = EmptyList[T]()) extends LinkedList[T] {
  private def detachLast(l : LinkedList[T]): LinkedList[T] = {
    l match {
      case EmptyList() => throw new Exception
      case b : ListBox[T] => {
        b.next match {
          case EmptyList() => EmptyList()
          case _ => ListBox(value, detachLast(b.next))
        }
      }
    }
  }
  private def getLast(l : LinkedList[T]): ListBox[T] = {
    l match {
      case EmptyList() => throw new Exception
      case b : ListBox[T] => {
        b.next match {
          case EmptyList() => b
          case _ => getLast(b.next)
        }
      }
    }
  }
  def map[B](func: T => B) : LinkedList[B] =
    new ListBox[B](func(value), next.map(func))
  def head = value
  def tail = next
  def last = getLast(this).value
  def init = detachLast(this)

  def ::: (l: LinkedList[T]) : LinkedList[T] =
    (l :+ value) ::: next
  def :+ (v : T) : LinkedList[T] = ListBox(value, next :+ v)

  override def toString: String = s"$value -> $next"
}
val a = EmptyList() :+ 1 :+ 2
val b = EmptyList() :+ 3 :+ 4
println(a ::: b)
