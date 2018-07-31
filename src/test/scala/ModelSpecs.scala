/**
  * implemented by Alexander Ray (info@alexray.me)
  */

import de.kaufhof.exercises.logs.model.Model._
import de.kaufhof.exercises.logs.model.ModelTypeClasses._
import de.kaufhof.exercises.logs.model.ModelStringPrinterImplicits._
import org.scalatest._
import de.kaufhof.exercises.logs.model.ModelBehaviorImplicits._

class ModelSpecs extends FlatSpec with Matchers {

  "Model printer" should "print teaser endpoint" in {
    val e: EndPoint = Teaser
    e.print should be ("teaser")
  }

  it should "print server name" in {
    Server(2).print should be ("srv2.kaufhof.de")
  }

  it should "print correct url" in {
    Url(Server(1), Profile).print should be ("http://srv1.kaufhof.de/profile.json")
  }

  it should "print correct duration" in {
    Duration(20).print should be ("20 ms")
  }

  "Summary" should "print correct entity summary" in {
    EntitySummary(100, 4, 1).print should be("avg: 25 ms, count: 4x, success 75% (1 of 4 failed)")
  }

  it should "print correct empty server summary" in {
    LogServerSummary(Map()).print should be ("")
  }

  it should "print correct server summary" in {
    val result = LogServerSummary(
      Map(
        Teaser    -> EntitySummary(100, 4, 1),
        Products  -> EntitySummary(500, 5, 3),
        Profile   -> EntitySummary(200, 10, 5)
      )
    ).print

    val compareResult =
      """        teaser avg: 25 ms, count: 4x, success 75% (1 of 4 failed)
        |        products avg: 100 ms, count: 5x, success 40% (3 of 5 failed)
        |        profile avg: 20 ms, count: 10x, success 50% (5 of 10 failed)""".stripMargin

    result should be(compareResult)
  }

  it should "print correct empty log summary" in {
    val compareResult =
      """
        |processed 0 log messages, parsed 0%
        |
        |""".stripMargin
    LogSummary(EntitySummary.empty, Map()).print should be(compareResult)
  }

  it should "print correct log summary" in {
    val srv1 = LogServerSummary(
      Map(
        Teaser    -> EntitySummary(100, 4, 1),
        Products  -> EntitySummary(500, 5, 3),
        Profile   -> EntitySummary(200, 10, 5)
      )
    )

    val srv2 = LogServerSummary(
      Map(
        Teaser    -> EntitySummary(300, 40, 6),
        Products  -> EntitySummary(590, 15, 3),
        Profile   -> EntitySummary(120, 10, 5)
      )
    )

    val log = LogSummary(EntitySummary(0, 2, 0),
      Map(
        Server(1) -> srv1,
        Server(2) -> srv2
      )
    )

    val result = log.print

    val compareResult =
      """
        |processed 2 log messages, parsed 100%
        |
        |srv1.kaufhof.de avg: 42 ms, count: 19x, success 52% (9 of 19 failed)
        |    endpoints:
        |        teaser avg: 25 ms, count: 4x, success 75% (1 of 4 failed)
        |        products avg: 100 ms, count: 5x, success 40% (3 of 5 failed)
        |        profile avg: 20 ms, count: 10x, success 50% (5 of 10 failed)
        |
        |srv2.kaufhof.de avg: 15 ms, count: 65x, success 78% (14 of 65 failed)
        |    endpoints:
        |        teaser avg: 7 ms, count: 40x, success 85% (6 of 40 failed)
        |        products avg: 39 ms, count: 15x, success 80% (3 of 15 failed)
        |        profile avg: 12 ms, count: 10x, success 50% (5 of 10 failed)
        |""".stripMargin

    result should be(compareResult)

  }

}

