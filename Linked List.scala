import scala.annotation.tailrec
import scala.util.control.Exception

sealed abstract class LinkedList[T] {
  def map[B](func: T => B) : LinkedList[B]
  def ::: (l: LinkedList[T]) : LinkedList[T]
  def :+ (v : T) : LinkedList[T]
  override def toString: String
}

case class EmptyBox[T]() extends LinkedList[T] {
  def map[B](func: T => B) : LinkedList[B] = EmptyBox[B]()
  def ::: (l: LinkedList[T]) : LinkedList[T] = l
  def :+ (v : T) : LinkedList[T] = ListBox[T](v)
  override def toString() = "Empty"
}
case class ListBox[T](value : T,
                      next : LinkedList[T] = EmptyBox[T]()) extends LinkedList[T] {
  private def detachLast(l : LinkedList[T]): LinkedList[T] = {
    l match {
      case EmptyBox() => throw new Exception
      case b : ListBox[T] => {
        b.next match {
          case EmptyBox() => EmptyBox()
          case _ => ListBox(value, detachLast(b.next))
        }
      }
    }
  }
  private def getLast(l : LinkedList[T]): ListBox[T] = {
    l match {
      case EmptyBox() => throw new Exception
      case b : ListBox[T] => {
        b.next match {
          case EmptyBox() => b
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

  def ::: (l: LinkedList[T]) : LinkedList[T] = ListBox(value, next ::: l)
  def :+ (v : T) : LinkedList[T] = ListBox(value, next :+ v)

  override def toString: String = s"$value -> $next"
}

val a = EmptyBox() :+ 1 :+ 1 :+ 1
println(a)
