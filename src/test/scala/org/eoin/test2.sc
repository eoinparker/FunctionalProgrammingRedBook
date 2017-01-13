import scala.util.{Failure, Success, Try}

object Async extends App {

    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration.Duration
    import scala.concurrent.{Await, Future}

    def m1: Future[Try[String]] = m3.map(_  map  {"a" + _})
    def m2: Future[Try[String]] = m3.map(_  map  {"b" + _})
    def m3: Future[Try[String]] = Future(Try {throw new Exception})

    val output = Await.result(m1, Duration.Inf)
    output match {
      case Failure(e) => e.printStackTrace()
      case Success(s) => println(s"success! + $s")
    }
}

Async.main(Array())