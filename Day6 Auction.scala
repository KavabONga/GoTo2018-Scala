import akka.actor._

import scala.concurrent.duration._
import akka.pattern.{ask, pipe}
import akka.util.Timeout
import com.typesafe.scalalogging.LazyLogging

import scala.collection.mutable
import akka.actor._
import akka.pattern.{ask, pipe}
import akka.util.Timeout

import scala.collection._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.io.StdIn

case class AddItem(name : String, startingPrice : Int = 0)
case class BetOnItem(item : String, price : Int)

class AuctionActor extends Actor with ActorLogging {
  val bets : mutable.Map[String, Int] = collection.mutable.Map.empty
  override def receive: Receive = {
    case AddItem(name, startingPrice) => {
      if (bets.contains(name))
        log.info(s"Item $name is already in Auction and it's current bet is ${bets(name)}")
      else {
        bets += name -> startingPrice
        log.info(s"Item $name with starting price $startingPrice is added to the Auction")
      }
    }
    case BetOnItem(item, price) if !bets.contains(item) => log.info(s"No item $item")
    case BetOnItem(item, price) if bets.contains(item) => {
      val curPrice = bets(item)
      if (curPrice > price) log.info(s"Someone already bet $curPrice on $item")
      else {
        bets.update(item, price)
        log.info(s"You successfully bet $price on $item")
      }
    }
    case r => log.warning(s"Unknown message:$r")
  }
}

object Main extends App {
  val actorSystem = ActorSystem("main-actor")
  val auction = actorSystem.actorOf(Props(new AuctionActor), "Auction")
  auction ! AddItem("Old lamp", 12)
  auction ! BetOnItem("Old lamp", 10)
  auction ! BetOnItem("fork", 100000)
}
