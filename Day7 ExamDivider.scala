import org.json4s.jackson.JsonMethods.{compact, parse, render}
import org.json4s.JsonDSL._
import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props, Status}
import akka.http.scaladsl.server.{Route, StandardRoute}
import akka.http.scaladsl.{Http, server}
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import org.json4s.{DefaultFormats, JsonDSL}
import org.json4s.jackson.{Json, JsonMethods}
import JsonMethods._

import scala.xml._
import scala.xml.Utility.trim
import scala.io.StdIn
import scala.util._
import akka.http.javadsl.model
import akka.http.scaladsl.unmarshalling.Unmarshal
import org.json4s.JsonAST._

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.duration.Duration._
import scala.concurrent.ExecutionContext.Implicits.global
import collection.mutable
import scala.xml.Utility._
import scala.xml._
import akka.pattern._
import akka.util.Timeout

case class AddDivider(depth : Int = 1)
case class Divide(n : Double)

class DivideActor(nDiff : Int) extends Actor with ActorLogging {
  var underDivider : ActorRef = ActorRef.noSender
  implicit val timeout = Timeout(3 seconds)
  override def receive: Receive = {
    case AddDivider(depth) => {
      underDivider = context.actorOf(Props(classOf[DivideActor], nDiff + 1), s"divider-${nDiff + 1}")
      if (depth - 1 > 0)
        underDivider ! AddDivider(depth - 1)
    }
    case Divide(x) => {
      val tryRes = Try(x / (x - nDiff))
      tryRes match {
        case Success(thisRes) => {
          if (underDivider == null) {
            log.info("reached the end")
            sender ! thisRes
          }
          else {
            log.info("Sending request")
            val tryActual = underDivider ? Divide(thisRes)
            Try(Await.result(tryActual, 3 seconds)) match {
              case Success(ans) => sender ! ans
              case Failure(_) => log.warning("Didn't get the response from underlying DivideActor")
            }
          }
        }
        case Failure(exception) => log.warning(s"Couldn't compute $x / ($x - $nDiff)")
      }
    }
  }
}

object Jsoner {
  def success(computationResult : String) = {
    val m = Map("success" -> "true", "message" -> computationResult)
    pretty(render(m))
  }
  def error(exception : Throwable) = {
    val m = Map("success" -> "false", "message" -> exception.getMessage)
    pretty(render(m))
  }
}
object DivideExam extends App {
  def successResponse(computationResult : String): StandardRoute =
    complete(HttpEntity(ContentTypes.`application/json`, Jsoner.success(computationResult)))
  def errorResponse(exception : Throwable): StandardRoute =
    complete(HttpEntity(ContentTypes.`application/json`, Jsoner.error(exception)))
  implicit val system = ActorSystem("my-system")
  implicit val dispatcher = system.dispatcher
  implicit val materializer = ActorMaterializer()
  implicit val timeout = Timeout(3 second)
  val mainDivider = system.actorOf(Props(classOf[DivideActor], 1), "divider-1")
  mainDivider ! AddDivider(2)

  val route =
    pathPrefix("divide") {
      pathPrefix(Remaining) { r =>
        val n = Try(r.toDouble)
        n match {
          case Success(value) => {
            val resFuture = mainDivider ? Divide(value)
            Try(Await.result(resFuture, 5 seconds)) match {
              case Success(result) => successResponse(result.toString)
              case Failure(exception) => errorResponse(exception)
            }
          }
          case Failure(exception) => errorResponse(exception)
        }
      }
    }
  val bindingFuture = Http().bindAndHandle(route, "localhost", 8080)

  println("Server ready")

}
