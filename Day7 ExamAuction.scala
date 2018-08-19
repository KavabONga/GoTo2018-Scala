import org.json4s.jackson.JsonMethods.{compact, parse, render}
import org.json4s.JsonDSL._
import akka.actor.ActorSystem
import akka.http.scaladsl.server.Route
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

case class AuctionResponse(message : String)

object Auction {
  val bets = mutable.Map.empty[String, Int]
  def add(item : String, startingPrice : Int) = {
    if (bets.contains(item))
      throw new Exception(s"$item already in Auction")
    else {
      bets += item -> startingPrice
      AuctionResponse(s"$item successfully added with starting price $startingPrice$$")
    }
  }
  def buy(item : String, price : Int) = {
    if (!bets.contains(item))
      throw new Exception(s"We don't have $item")
    else {
      val curPrice = bets(item)
      if (curPrice > price)
        throw new Exception(s"You will have to pay at least $curPrice$$ for $item")
      else {
        bets -= item
        AuctionResponse(s"You successfully bought $item for $price$$")
      }
    }
  }
  def raise(item : String, price : Int) = {
    if (!bets.contains(item))
      throw new Exception(s"We don't have $item")
    else {
      val curPrice = bets(item)
      if (curPrice > price)
        throw new Exception(s"Someone already bet $curPrice$$ on $item")
      else {
        bets.update(item, price)
        AuctionResponse(s"You successfully raised $curPrice$$ to $price$$ on $item")
      }
    }
  }
  def remove(item : String) = {
    if (bets.contains(item)) {
      bets -= item
      AuctionResponse(s"$item was removed from the Auction")
    }
    else
      throw new Exception(s"We don't have $item")
  }
}
object Jsoner {
  def success(message : String) = {
    val m = Map("success" -> "true", "message" -> message)
    pretty(render(m))
  }
  def error(exception : Throwable) = {
    val m = Map("success" -> "false", "message" -> exception.getMessage)
    pretty(render(m))
  }
}
object AuctionExam extends App {
  implicit val system = ActorSystem("my-system")
  implicit val dispatcher = system.dispatcher
  implicit val materializer = ActorMaterializer()
  val route =
    pathPrefix("main") {
      pathEnd {
        complete(trim {
          <message>
            {"This is the home page"}
          </message>
        }.toString)
      } ~
        pathPrefix("set") {
          pathPrefix(Remaining) { r =>
            val tryArgs = Try {
              val args = r.split('_')
              (args(0), args(1).toInt)
            }
            tryArgs match {
              case Success(args) => {
                val trySet = Try(Auction.add(args._1, args._2).message)
                trySet match {
                  case Success(message) =>
                    complete(HttpEntity(ContentTypes.`application/json`,Jsoner.success(message)))
                  case Failure(exc) =>
                    complete(HttpEntity(ContentTypes.`application/json`,Jsoner.error(exc)))
                }

              }
              case Failure(exc) =>
                complete(HttpEntity(ContentTypes.`application/json`,Jsoner.error(exc)))
            }
          }
        } ~
          pathPrefix("raise") {
            pathPrefix(Remaining) { r =>
              val tryArgs = Try {
                val args = r.split('_')
                (args(0), args(1).toInt)
              }
              tryArgs match {
                case Success(args) => {
                  val tryRaise = Try(Auction.raise(args._1, args._2).message)
                  tryRaise match {
                    case Success(message) =>
                      complete(HttpEntity(ContentTypes.`application/json`,Jsoner.success(message)))
                    case Failure(exc) =>
                      complete(HttpEntity(ContentTypes.`application/json`,Jsoner.error(exc)))
                  }

                }
                case Failure(exc) =>
                  complete(HttpEntity(ContentTypes.`application/json`,Jsoner.error(exc)))
              }
            }
          } ~
            pathPrefix("buy") {
              pathPrefix(Remaining) { r =>
                val tryArgs = Try {
                  val args = r.split('_')
                  (args(0), args(1).toInt)
                }
                tryArgs match {
                  case Success(args) => {
                    val tryBuy = Try(Auction.buy(args._1, args._2).message)
                    tryBuy match {
                      case Success(message) =>
                        complete(HttpEntity(ContentTypes.`application/json`,Jsoner.success(message)))
                      case Failure(exc) =>
                        complete(HttpEntity(ContentTypes.`application/json`,Jsoner.error(exc)))
                    }

                  }
                  case Failure(exc) =>
                    complete(HttpEntity(ContentTypes.`application/json`,Jsoner.error(exc)))
                }
              }
            } ~
              pathPrefix("remove") {
                pathPrefix(Remaining) { r =>
                  val tryRemove = Try(Auction.remove(r).message)
                  tryRemove match {
                    case Success(message) => complete(HttpEntity(ContentTypes.`application/json`,Jsoner.success(message)))
                    case Failure(exc) =>
                      complete(HttpEntity(ContentTypes.`application/json`,Jsoner.error(exc)))
                  }
                }
              } ~
                path("end") {
                  system.terminate()
                  complete(HttpEntity(ContentTypes.`application/json`,Jsoner.success("System terminated")))
                }
    }
  val bindingFuture = Http().bindAndHandle(route, "localhost", 8080)

  println("Server ready")

}
