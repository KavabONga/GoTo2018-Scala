import scala.util._
import scala.concurrent._
import scala.concurrent.duration._
import com.typesafe.scalalogging.LazyLogging

import scala.concurrent.ExecutionContext.Implicits.global

object UsefulFuture extends LazyLogging {
  def safeDiv[T](a : Double, b : Double) : Double =
    Try(Await.result( Future {a / b}, Duration(10, SECONDS))) match {
      case Success(value) => value
      case Failure(_) => {
        logger.error(s"Tried to divide $a by $b")
        throw new Exception
      }
    }
  def bypassOnComplete[A, T](f : Future[T], func: T => A) : Future[A] = f.map(func)
  def ifFailure[T](f : Future[T], funcFail: => Unit, exc : Option[Throwable] = None) : T = {
    Try(Await.result( f, Duration(10, SECONDS))) match {
      case Failure(_) => {
        funcFail
        throw exc.get
      }
      case Success(value) => value
    }
  }
  def completeAndThen[A, B](firstFuture : Future[A], lastFuture : Future[B]) = {
    Try(Await.result(firstFuture, Duration(10, SECONDS))) match {
      case Success(_) => lastFuture
      case Failure(exc) => throw exc
    }
  }
  def completeAndThenComplete[A, B](firstFuture : Future[A], lastFuture : Future[B]) = {
    Try(Await.result(firstFuture, Duration(10, SECONDS))) match {
      case Success(_) => {
        Try(Await.result(lastFuture, Duration(10, SECONDS))) match {
          case Success(value) => value
          case Failure(exc) => throw exc
        }
      }
      case Failure(exc) => throw exc
    }
  }
}

val f = UsefulFuture.completeAndThenComplete(Future {10}, Future {20})
println(f)
