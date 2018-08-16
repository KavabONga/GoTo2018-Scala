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
  def bypassOnComplete[A, T](f : Future[T], func: T => A) : Future[A] = {
    Try(Await.result( f, Duration(10, SECONDS))) match {
      case Success(_) => f.map(func)
      case Failure(exception) => Future.failed(exception)
    }
  }
  def ifFailure[T](f : Future[T], funcFail: => Unit, exc : Option[Throwable] = None) : Future[T] = {
    Try(Await.result( f, Duration(10, SECONDS))) match {
      case Failure(ex) => {
        funcFail
        Future.failed(exc.getOrElse(ex))
      }
      case Success(value) => f
    }
  }
  def completeAndThen[T](firstFuture : Future[T], lastFuture : Future[T]) = {
    Try(Await.result(firstFuture, Duration(10, SECONDS))) match {
      case Success(_) => lastFuture
      case Failure(exc) => firstFuture
    }
  }
  def completeAndThenComplete[T](firstFuture : Future[T], lastFuture : Future[T]) = {
    Try(Await.result(firstFuture, Duration(10, SECONDS))) match {
      case Success(_) => {
        Try(Await.result(lastFuture, Duration(10, SECONDS))) match {
          case Success(value) => lastFuture
          case Failure(exc) => Future.failed(exc)
        }
      }
      case Failure(_) => firstFuture
    }
  }
}

val f = UsefulFuture.completeAndThenComplete(Future {10}, Future {20})
println(f)
