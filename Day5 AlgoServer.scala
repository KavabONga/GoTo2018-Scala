  package Main
  import org.json4s.jackson.JsonMethods.{compact, render}
  import org.json4s.JsonDSL._
  import akka.actor.ActorSystem
  import akka.http.scaladsl.server.Route
  import akka.http.scaladsl.{Http, server}
  import akka.http.scaladsl.model._
  import akka.http.scaladsl.server.Directives._
  import akka.stream.ActorMaterializer
  import org.json4s.JsonDSL
  import org.json4s.jackson.Json

  import scala.io.StdIn
  import scala.util._


  object Algo {
    def UrlStringToArray(s : String) = s.split('+').map(_.toInt)
    def UrlStringToArguments(s : String) = s.split('_')
    def rotateArray[P](k : Int, l : List[P]) = {
      val offset = (k % l.length + l.length) % l.length
      l.takeRight(l.length - offset) ::: l.take(offset)
    }
    def binPow(x : Int, n : Int): Int = {
      if (n == 0) 1
      else {
        if (n % 2 == 0) {
          val t = binPow(x, n / 2)
          t * t
        }
        else binPow(x, n - 1) * x
      }
    }
    def bubbleSort(l : List[Int]) : List[Int] = {
      if (l.isEmpty) List()
      else {
        val newL = l.tail.foldLeft(List(l.head))((ar : List[Int], x) => {
          if (ar.last > x) ar.init :+ x :+ ar.last
          else ar :+ x
        })
        bubbleSort(newL.init) :+ newL.last
      }
    }
  }

  object Main extends App{

    def nameToJson(name: String, surname : String) = {
      val serialized = Map("name" -> name, "surname" -> surname)
      compact(render(JsonDSL.map2jvalue(serialized)))
    }
    def argParseErrorString(args : String) = s"You probably entered the arguments wrong ($args)"
    def argParseErrorPage(args : String) = complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, argParseErrorString(args)))
    implicit val system = ActorSystem("my-system")
    implicit val materializer = ActorMaterializer()
    // needed for the future flatMap/onComplete in the end
    implicit val executionContext = system.dispatcher
    val route: Route =
      pathPrefix("remaining") {
        pathPrefix(Remaining) { name =>
          complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, s"You just entered the $name page.<br>Have fun."))
        }
      } ~
        pathPrefix("algorithms") {
          pathPrefix("rotateArray") {
            pathPrefix(Remaining) { argsLine =>
              val allTry = Try({
                val args = Algo.UrlStringToArguments(argsLine)
                val l = Algo.UrlStringToArray(args(0)).toList
                val r = args(1).toInt
                val resL = Algo.rotateArray(r, l)
                complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, s"Array ${l.mkString("[", ",", "]")} rotated by $r is ${resL.mkString("[", ",", "]")}"))
              })
              allTry match {
                case Success(entity) => entity
                case Failure(_) => argParseErrorPage(argsLine)
              }
            }
          } ~
            pathPrefix("binaryPower") {
              pathPrefix(Remaining) { argsLine =>
                val allTry = Try({
                  val args = Algo.UrlStringToArguments(argsLine)
                  val x = args(0).toInt
                  val p = args(1).toInt
                  val res = Algo.binPow(x, p)
                  complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, s"$x powered by $p equals $res"))
                })
                allTry match {
                  case Success(resp) => resp
                  case Failure(_) => argParseErrorPage(argsLine)
                }
              }
            } ~
              pathPrefix("bubbleSort") {
                pathPrefix(Remaining) { argsLine =>
                  val allTry = Try({
                    val l = Algo.UrlStringToArray(argsLine).toList
                    val sortedL = Algo.bubbleSort(l)
                    complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, s"${l.mkString("[", ",", "]")} sorted is ${sortedL.mkString("[", ",", "]")}"))
                  })
                  allTry match {
                    case Success(resp) => resp
                    case Failure(_) => argParseErrorPage(argsLine)
                  }
                }
              }
        } ~
        get {
          complete("Woops, you got 404")
        }
    val bindingFuture = Http().bindAndHandle(route, "localhost", 8080)

    println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
    StdIn.readLine() // let it run until user presses return
    bindingFuture
      .flatMap(_.unbind()) // trigger unbinding from the port
      .onComplete(_ => system.terminate()) // and shutdown when done
  }
