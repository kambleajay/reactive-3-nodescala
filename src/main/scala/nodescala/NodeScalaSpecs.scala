package nodescala

import org.scalacheck.Properties
import org.scalacheck.Prop.{forAll, throws}
import scala.concurrent.{Await, Future}
import scala.language.postfixOps
import scala.concurrent.duration._
import java.util.concurrent.TimeoutException

class NodeScalaSpecs extends Properties("NodeScala") {

  property("always returns with given value") = forAll { (aVal: AnyVal) =>
    val actual = Future.always(aVal)
    Await.result(actual, 1 nanos) == aVal
  }

  property("never does not create future") = forAll { (x: String) =>
    val never = Future.never[x.type]
    throws(classOf[TimeoutException])(Await.result(never, 1 nanos))
  }

}

object Checks {

  def main(args: Array[String]) {
    new NodeScalaSpecs().check
  }

}
