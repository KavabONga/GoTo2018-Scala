import org.json4s.jackson.JsonMethods.{compact, parse, render}
import org.json4s.JsonDSL._
import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, PoisonPill, Props, Status}
import akka.http.scaladsl.server.{Route, StandardRoute}
import akka.http.scaladsl.{Http, server}
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import org.json4s.{DefaultFormats, JsonDSL}
import org.json4s.jackson.{Json, JsonMethods}
import JsonMethods._
import concurrent.Future
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

case object AreYouTelegram
case class YouHaveToCheckThem(l : List[ActorRef])

class RCO extends Actor with ActorLogging {
  implicit val timeout = Timeout(2, SECONDS)
  var done = false
  override def receive: Receive = {
    case _ if done => log.info("My work is done")
    case YouHaveToCheckThem(l) => {
      val checksOnSites = Future.sequence(l.map(act => act ? AreYouTelegram))
      Await.result(checksOnSites, 2 seconds).zip(l).foreach(ans => {
        ans._1 match {
          case "yes" => {
            ans._2 ! PoisonPill
            log.info(s"${ans._2.path.name} is ded :)")
          }
          case "no" => {
            ans._2 ! PoisonPill
            log.info(s"${ans._2.path.name} is ded :)")
          }
          case r => log.info(s"""What's "$r", please help""")
        }
      })
      done = true
    }
  }
}

class WebSite extends Actor with ActorLogging {
  override def receive: Receive = {
    case AreYouTelegram => {
      val randomDestinyDecider = Random.nextDouble()
      log.info(s"Just got asked by ${sender.path.name} whether I am Telegram or not))) This will definitely not cause any problems))")
      if (Set(1.0, 0.0, 0.666, 0.179, 0.444).contains(randomDestinyDecider))
        sender ! "Haha, not today"
      else {
        if (randomDestinyDecider < 0.5)
          sender ! "yes"
        else
          sender ! "no"
      }
    }
  }
}

object RCOExam extends App {
  implicit val system = ActorSystem()
  implicit val dispatcher = system.dispatcher
  implicit val materializer = ActorMaterializer()
  implicit val timeout = Timeout(1, SECONDS)
  val rco = system.actorOf(Props(new RCO), "RCO")
  val google = system.actorOf(Props(new WebSite), "Google")
  val rambler = system.actorOf(Props(new WebSite), "Rambler")
  val yandex = system.actorOf(Props(new WebSite), "Yandex")
  rco ! YouHaveToCheckThem(List(google, rambler, yandex))

}
