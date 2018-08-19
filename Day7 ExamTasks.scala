import scala.concurrent._
import scala.util._
import ExecutionContext.Implicits.global

def hardMult(l: List[Future[Try[Option[Int]]]], k : Int = 10) = {
  l.map(f => {
    f.map(t => {
      t.map(p => p.map(_ * k))
    })
  })
}
val a = hardMult(
  List.fill(3)(Future {Try(Option(2 / 1))}) ::: List.fill(3)(Future {Try(Option(2 / 0))})
)
Thread.sleep(100)
println(a)
def hardCountNone(l : List[Try[Option[Any]]]) = {
  l.count(t => {
    t.isSuccess && t.get.isEmpty
  })
}
println(hardCountNone(
  List.fill(5)(Try(Option.empty[Any])) ::: List.fill(5)(Try(Option(1)))
))
