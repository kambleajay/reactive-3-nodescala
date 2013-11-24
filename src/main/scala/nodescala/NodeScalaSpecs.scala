package nodescala

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import scala.concurrent.{Await, Future}
import scala.language.postfixOps
import scala.concurrent.duration._

class NodeScalaSpecs extends Properties("NodeScala") {

  property("always returns with given value") = forAll { (aVal: AnyVal) =>
    val actual = Future.always(aVal)
    Await.result(actual, 1 nanos) == aVal
  }

}

object Checks {

  def main(args: Array[String]) {
    new NodeScalaSpecs().check
  }

}
