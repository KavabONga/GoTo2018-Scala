sealed abstract class Queue[T] {
  override def toString: String
  def :+(v : T) : Queue[T]
}

case class EmptyQueue[T]() extends Queue[T] {
  override def toString: String = "end"
  def :+(v : T) : Queue[T] = QueueBox[T](v)
}
case class QueueBox[T](value : T, next : Queue[T] = EmptyQueue[T]()) extends Queue[T]{
  private def detachLast(l : QueueBox[T]): Queue[T] =
      l.next match {
        case EmptyQueue() => EmptyQueue()
        case b : QueueBox[T] => QueueBox(value, detachLast(b))
      }
  private def getLast(q : QueueBox[T]): QueueBox[T] =
    q.next match {
      case EmptyQueue() => q
      case b : QueueBox[T] => getLast(b)
    }
  def back : T  = value
  def front : T = getLast(this).value
  def tail : Queue[T] = pop()
  def init : Queue[T] = next
  def push(v : T): Queue[T] =
    QueueBox(v, this)
  def pop(): Queue[T] =
    detachLast(this)
  def :+(v : T): Queue[T] = push(v)

  override def toString: String = s"$value:$next"
}

val a = EmptyQueue() :+ 1 :+ 1
println(a)
