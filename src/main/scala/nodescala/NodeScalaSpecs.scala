package nodescala

import org.scalacheck.{Gen, Properties}
import org.scalacheck.Prop._
import scala.concurrent.{Promise, Await, Future}
import scala.language.postfixOps
import scala.concurrent.duration._
import java.util.concurrent.TimeoutException

class NodeScalaSpecs extends Properties("NodeScala") {

  property("always") = forAll {
    (aVal: AnyVal) =>
      val actual = Future.always(aVal)
      Await.result(actual, 1 nanos) == aVal
  }

  property("never") = forAll {
    (x: String) =>
      val never = Future.never[x.type]
      throws(classOf[TimeoutException])(Await.result(never, 1 nanos))
  }

  property("all") = forAll {
    (xs: List[String]) =>
      val listOfFutures: List[Future[String]] = xs map (x => Future.always(x))
      val allInFuture: Future[List[String]] = Future.all(listOfFutures)
      Await.result(allInFuture, 1 second) == xs
  }

  val durationGen = for {
    length <- Gen.choose(5, 25)
    time = MILLISECONDS
  } yield Duration(length, time)

  property("delay") = forAll(durationGen) {
    (dur: Duration) =>
      val f = Future.delay(dur)
      throws(classOf[TimeoutException])(Await.result(f, dur - Duration(1, SECONDS)))
  }

  val numsAnySuccess = for { x <- Gen.choose(1, 10); y <- Gen.choose(1, 10) } yield(x, y)
  property("any:success") = forAll(numsAnySuccess) { (nums: (Int, Int)) =>
    val p1 = Promise[Int]()
    val p2 = Promise[Int]()
    val result = Future.any(List(p1.future, p2.future))
    p2.success(nums._2)
    Await.result(result, 1 second) == nums._2
  }

  val dataNowYes = Gen.choose(1, 100)
  property("now:yes") = forAll(dataNowYes) { (x: Int) =>
    val f = Future.always(x)
    f.now == x
  }

  property("now:no") = forAll(dataNowYes) { (x: Int) =>
    val f = Future.delay(Duration(5, MILLISECONDS))
    throws(classOf[NoSuchElementException])(f.now)
  }

}

object Checks {

  def main(args: Array[String]) {
    new NodeScalaSpecs().check
  }

}