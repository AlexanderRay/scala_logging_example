/**
  * implemented by Alexander Ray (info@alexray.me)
  */

import akka.NotUsed
import akka.stream.scaladsl.Source
import de.kaufhof.exercises.logs.model.Model._
import org.scalatest.{FlatSpec, Matchers}
import de.kaufhof.exercises.logs.model.ModelTypeClasses._
import de.kaufhof.exercises.logs.model.ModelStringPrinterImplicits._
import de.kaufhof.exercises.logs.processor.LogProcessor
import de.kaufhof.exercises.logs.model.ModelBehaviorImplicits._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Seconds, Span}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import cats.implicits._
import de.kaufhof.exercises.logs.LogAnalyzer.LogProducer
import de.kaufhof.exercises.logs.parser.LogEntityParser
import org.scalatest.exceptions.TestFailedException

class ProcessorSpecs extends FlatSpec with Matchers with ScalaFutures {

  "Async test" should "correct recognize result" in {
    val f: Future[Int] = Future {
      10
    }

    whenReady(f) { r =>
      r should be(10)
    }

  }

  it should "fail on timeout" in {
    val f: Future[Int] = Future {
      while (true) {}

      10
    }

    intercept[TestFailedException] {
      whenReady(f, timeout(Span(1, Seconds))) { r =>
        r should be (10)
      }
    }

  }

  "Processor" should "correctly process empty source" in {
    val source = Source(List[String]())
    val result: Future[LogSummary] = LogProcessor().process(source)

    whenReady(result, timeout(Span(5, Seconds))) { ls =>
      assert(ls == LogSummary.empty)
    }
  }

  it should "correctly process source of one element" in {

    val l1 = "12:14:55:674 INFO - fetch http://srv1.kaufhof.de/profile.json took: 300 ms"

    val source = Source(List(l1))
    val le = LogEntry(LogSuccess, Url(Server(1), Profile), Duration(300))
    val result = LogSummary.empty |+| LogSummary(le)

    whenReady(LogProcessor().process(source), timeout(Span(5, Seconds))) { ls =>
      assert(ls == result)
    }
  }

  it should "correctly process source of two elements" in {

    val l1 = "12:14:55:674 INFO - fetch http://srv1.kaufhof.de/profile.json took: 300 ms"
    val l2 = "12:14:55:674 WARN - fetch http://srv2.kaufhof.de/teaser.json took: 100 ms"

    val source = Source(List(l1, l2))
    val result = LogSummary.empty |+|
      LogSummary(LogEntry(LogSuccess, Url(Server(1), Profile), Duration(300))) |+|
      LogSummary(LogEntry(LogFailed,  Url(Server(2), Teaser), Duration(100)))

    whenReady(LogProcessor().process(source), timeout(Span(5, Seconds))) { ls =>
      assert(ls == result)
    }
  }

  it should "correctly process source of 1000 random elements" in {

    val logList = new LogProducer(1000).logs.toList

    val syncResult = logList.map(logEntry => LogEntityParser(logEntry))
      .foldLeft(LogSummary.empty) { case (logSummary, parseResult) =>
        logSummary |+| parseResult.fold({_ => LogSummary.failed}, { logEntity => LogSummary(logEntity)})
      }

    whenReady(LogProcessor().process(Source(logList)), timeout(Span(5, Seconds))) { ls =>
      ls should be (syncResult)
    }

  }

}
