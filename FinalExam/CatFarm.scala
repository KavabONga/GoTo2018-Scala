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

case class Cat(name: String, isMale: Boolean, breed: String, age: Int)

object Jsoner {
  def success(comment : String = "")= {
    pretty(render(JObject(JField("success", true), JField("comment", comment))))
  }
  def error(exc : Throwable) = {
    pretty(render(JObject(JField("success", false), JField("message", exc.getMessage))))
  }
}

object Responser {
  def success(comment : String = "") = {
    complete(HttpEntity(ContentTypes.`application/json`, Jsoner.success(comment)))
  }
  def error(exc :Throwable) = {
    complete(HttpEntity(ContentTypes.`application/json`, Jsoner.error(exc)))
  }
}

case class NewCat(cat : Cat)
case class TakeCat(name : String)
case class MateCats(name1: String, name2 : String)

case class FarmSuccess(message : String)
case class FarmFailure(exc : Throwable)

class CatFarm extends Actor {
  val cats = mutable.Map[String, Cat]()
  override def receive: Receive = {
    case NewCat(cat) => {
      if (cats.contains(cat.name))
        sender ! FarmFailure(new Exception(s"We already have a cat with name ${cat.name}"))
      else {
        cats += cat.name -> cat
        sender ! FarmSuccess(s"You successfully added $cat")
      }
    }
    case TakeCat(name) => {
      if (!cats.contains(name))
        sender ! FarmFailure(new Exception(s"We don't have a cat with name $name"))
      else {
        val cat = cats.remove(name)
        sender ! FarmSuccess(s"You successfully took ${cat.get}")
      }
    }
    case MateCats(name1, name2) => {
      if (!cats.contains(name1))
        sender ! FarmFailure(new Exception(s"We don't have a cat with name $name1"))
      else {
        if (!cats.contains(name2))
          sender ! FarmFailure(new Exception(s"We don't have a cat with name $name2"))
        else {
          if (cats(name1).isMale == cats(name2).isMale)
            sender ! FarmFailure(new Exception(s"Sorry, we don'n know how to mate cats of same sex"))
          else {
            cats += (name1 + name2) -> Cat(
              name1 + name2,
              Random.nextBoolean(),
              if (Random.nextBoolean()) cats(name1).breed else cats(name2).breed,
              0
            )
            sender ! FarmSuccess(s"You mated cats ${cats(name1)} and ${cats(name2)} and got ${cats(name1 + name2)}")
          }
        }
      }
    }
  }
}

object CatExam extends App {
  implicit val system = ActorSystem()
  implicit val dispatcher = system.dispatcher
  implicit val materializer = ActorMaterializer()
  implicit val timeout = Timeout(1, SECONDS)
  val catFarm = system.actorOf(Props(new CatFarm))
  val route =
    pathPrefix("add") {
      pathPrefix(Remaining) { s =>
        val args = s.split('_')
        val catTry = Try(
          Cat(
            args(0),
            args(1).toBoolean,
            args(2),
            args(3).toInt
          )
        )
        catTry match {
          case Success(cat) => {
            val tryAdd = catFarm ? NewCat(cat)
            Await.result(tryAdd, 1 second) match {
              case FarmSuccess(status) => Responser.success(status.toString)
              case FarmFailure(exc) => Responser.error(exc)
              case r => complete("Unexpected behaviour")
            }
          }
          case Failure(exception) => Responser.error(exception)
        }
      }
    } ~
      pathPrefix("take") {
        pathPrefix(Remaining) { name =>
          val tryTake = catFarm ? TakeCat(name)
          Await.result(tryTake, 1 second) match {
            case FarmSuccess(status) => Responser.success(status.toString)
            case FarmFailure(exc) => Responser.error(exc)
            case r => complete("Unexpected behaviour")
          }
        }
      } ~
      pathPrefix("mate") {
        pathPrefix(Remaining) { s =>
          val args = s.split('_')
          val namesTry = Try(
            (
              args(0),
              args(1)
            )
          )
          namesTry match {
            case Success(names) => {
              val tryMate = catFarm ? MateCats(names._1,names._2)
              Await.result(tryMate, 1 second) match {
                case FarmSuccess(status) => Responser.success(status.toString)
                case FarmFailure(exc) => Responser.error(exc)
                case r => complete("Unexpected behaviour")
              }
            }
            case Failure(exception) => Responser.error(exception)
          }
        }
      }

  val bindingFuture = Http().bindAndHandle(route, "localhost", 8080)

  println("Server ready")


}
