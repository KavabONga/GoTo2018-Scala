import UsefulFuture.{logger, safeDiv}

import scala.util._
import scala.concurrent._
import scala.concurrent.duration._
import com.typesafe.scalalogging.LazyLogging

import scala.concurrent.ExecutionContext.Implicits.global

object UsefulFuture extends LazyLogging {
  def safeDiv[T](a : Double, b : Double) : Double =
    Try({Await.result( Future {a / b}, Duration(1, SECONDS))}) match {
      case Success(value) => value
      case Failure(_) => {
        logger.error(s"Tried to divide $a by $b")
        throw new Exception
      }
    }
  def bypassOnComplete[A, T](f : Future[T], func: T => A) : Future[A] = {
    f.map(func)
  }
  def ifFailure[T](f : Future[T], funcFail: => Unit, exc : Option[Throwable] = None) : Future[T] = {
    f.onComplete({
      case Failure(_) => {
        funcFail()
        throw exc
      }
    })
    f
  }
}

val f = UsefulFuture.bypassOnComplete(Future{10}, (x : Int) => x * 2)
println(Await.result(f, Duration.Inf))
