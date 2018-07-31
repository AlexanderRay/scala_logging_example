/**
  * implemented by Alexander Ray (info@alexray.me)
  *
  * a [[de.kaufhof.exercises.logs.model.ModelTypeClasses.Printer]] type class String representation
  */
package de.kaufhof.exercises.logs.model

import Model._
import ModelTypeClasses._
import ModelBehaviorImplicits._

object ModelStringPrinterImplicits {

  /**
    * String type class specification
    * @tparam T - print source
    */
  trait StringPrinter[T] extends Printer[T, String]

  /**
    * prints end point names
    */
  implicit object EndPointStringPrinter extends StringPrinter[EndPoint] {
    override def print(x: EndPoint): String = x match {
      case Teaser   => "teaser"
      case Profile  => "profile"
      case Products => "products"
    }
  }

  /**
    * prints server name in a form 'srv{server.id}.kaufhof.de'
    *
    */
  implicit object ServerPrinter extends StringPrinter[Server] {
    override def print(x: Server): String = s"srv${x.id}.kaufhof.de"
  }

  /**
    * prints a URL in a form: 'http://{server}/{enpoint}.json'
    */
  implicit object UrlPrinter extends StringPrinter[Url] {
    override def print(x: Url): String =
      s"http://${x.server.print}/${x.endPoint.print}.json"
  }

  /**
    * print duration in a form: '{xx} ms'
    */
  implicit object DurationPrinter extends StringPrinter[Duration] {
    override def print(x: Duration): String = s"${x.ms} ms"
  }

  /**
    * prints summary in a form:
    *
    * 'avg: {xx} ms, count: {xx}x, success {xx}% ({xx} of {xx} failed)'
    *
    * i.E. 'avg 200 ms, count 100, success 80% (20 of 100 failed)'
    *
    */
  implicit object EntitySummaryPrinter extends StringPrinter[EntitySummary] {
    def print(x: EntitySummary): String =
      s"avg: ${x.avgMs} ms, count: ${x.count}x, success ${x.successP}% (${x.failed} of ${x.count} failed)"
  }

  /**
    * print a summary per end point
    *
    */
  implicit object LogSummaryServerPrinter extends StringPrinter[LogServerSummary] {
    def print(x: LogServerSummary): String = x.es.map{e => s"        ${e._1.print} ${e._2.print}"}.mkString("\n")
  }

  /**
    * prints a complete log summary
    *
    * i.E.
    *
    * processed 10000 log messages, parsed 100%
    * srv1.kaufhof.de avg: 218 ms, count: 5015x, success 78% (1063 of 5015 failed)
    *     endpoints:
    *          products avg: 222 ms, count: 1637x, success 78% (352 of 1637 failed)
    *          profile avg: 218 ms, count: 1668x, success 79% (344 of 1668 failed)
    *          teaser avg: 216 ms, count: 1710x, success 78% (367 of 1710 failed)
    * srv2.kaufhof.de avg: 217 ms, count: 4985x, success 80% (952 of 4985 failed)
    *      endpoints:
    *          teaser avg: 218 ms, count: 1596x, success 80% (317 of 1596 failed)
    *          profile avg: 217 ms, count: 1697x, success 81% (321 of 1697 failed)
    *          products avg: 216 ms, count: 1692x, success 81% (314 of 1692 failed)
    *
    */
  implicit object LogSummaryPrinter extends StringPrinter[LogSummary] {
    def print(x: LogSummary): String = {
      s"\nprocessed ${x.processSummary.count} log messages, parsed ${x.processSummary.successP}%\n\n" +
      x.serverSummary.map{ case (k, v) =>
        List(
          s"${k.print} ${EntitySummary(v).print}",
          "    endpoints:",
          v.print,
          ""
        )
      }.toList.flatten.mkString("\n")
    }
  }

}
