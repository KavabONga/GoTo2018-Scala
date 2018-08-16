import scala.util._
import scala.concurrent._
import scala.concurrent.duration._
import com.typesafe.scalalogging.LazyLogging

import scala.concurrent.ExecutionContext.Implicits.global

object UsefulFuture extends LazyLogging {
  def safeDiv(a : Double, b : Double) : Double = {
    Try(a / b) match {
      case Success(value) => value
      case Failure(exc) => {
        logger.info("Welp, you've done it")
        throw exc
      }
    }
  }
  def bypassOnComplete[T](f : Future[T], func: T => T) : Future[T] = f.map(func)
  def ifFailure[T](f : Future[T], funcFail: => Unit, exc : Option[Throwable] = None) : Future[T] = {
    f transformWith {
      case Failure(ex) => {
        funcFail
        Future.failed(exc.getOrElse(ex))
      }
      case Success(_) => f
    }
  }
  def completeAndThen[T](firstFuture : Future[T], lastFuture : Future[T]) = {
    firstFuture transformWith {
      case Success(value) =>  {
        logger.info(s"First result is $value")
        lastFuture
      }
      case Failure(_) => {
        logger.info("First computation failed")
        firstFuture
      }
    }
  }
  def completeAndThenComplete[T](firstFuture : Future[T], lastFuture : Future[T]) = {
    firstFuture onComplete {
      case Success(value) =>  {
        logger.info(s"First result is $value")
        lastFuture onComplete {
          case Success(val2) => logger.info(s"Second result is $val2")
          case Failure(_)    => logger.info("Second computation failed")
        }
      }
      case Failure(_) => logger.info("First computation failed")
    }
  }
}
