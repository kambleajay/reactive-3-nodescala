package nodescala

import org.scalacheck.{Gen, Properties}
import org.scalacheck.Prop.{forAll, throws}
import scala.concurrent.{Await, Future}
import scala.language.postfixOps
import scala.concurrent.duration._
import java.util.concurrent.{TimeUnit, TimeoutException}

class NodeScalaSpecs extends Properties("NodeScala") {

  property("always") = forAll { (aVal: AnyVal) =>
    val actual = Future.always(aVal)
    Await.result(actual, 1 nanos) == aVal
  }

  property("never") = forAll { (x: String) =>
    val never = Future.never[x.type]
    throws(classOf[TimeoutException])(Await.result(never, 1 nanos))
  }

  property("all") = forAll { (xs: List[String]) =>
    val listOfFutures: List[Future[String]] = xs map (x => Future.always(x))
    val allInFuture: Future[List[String]] = Future.all(listOfFutures)
    Await.result(allInFuture, 1 millisecond) == xs
  }

  val durationGen = for {
    length <- Gen.choose(10, 100)
    time = TimeUnit.MILLISECONDS
  } yield Duration(length, time)

  property("delay") = forAll(durationGen) { (dur: Duration) =>
    val f = Future.delay(dur)
    throws(classOf[TimeoutException])(Await.result(f, dur - Duration(9, TimeUnit.MILLISECONDS)))
  }

}

object Checks {

  def main(args: Array[String]) {
    new NodeScalaSpecs().check
  }

}
