/**
  * implemented by Alexander Ray (info@alexray.me)
  *
  * Log Processor
  */
package de.kaufhof.exercises.logs.processor

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl._
import cats.implicits._
import de.kaufhof.exercises.logs.model.Model.LogSummary
import de.kaufhof.exercises.logs.model.ModelBehaviorImplicits._
import de.kaufhof.exercises.logs.parser.LogEntityParser
import scala.concurrent.{ExecutionContextExecutor, Future}

/**
  * a log processor - processes the logs via akka streams
  *
  * there are two ways to process, sync or async
  *
  */
case class LogProcessor() {

  implicit private val system: ActorSystem = ActorSystem("log-processor")
  implicit private val materializer: ActorMaterializer = ActorMaterializer()
  implicit private val ec: ExecutionContextExecutor = system.dispatcher

  /**
    * synchronous (means with sync-fold) way to process logs
    *
    * @param source - a  [[akka.stream.scaladsl.Source]] of string log entities
    * @return - [[scala.concurrent.Future]] with a [[de.kaufhof.exercises.logs.model.Model.LogSummary]]
    */
  def process(source: Source[String, NotUsed]): Future[LogSummary] = {

    source
      .map (logEntry => LogEntityParser(logEntry))
      .fold(LogSummary.empty) { case (logSummary, parseResult) =>
          logSummary |+| parseResult.fold({_ => LogSummary.failed}, { logEntity => LogSummary(logEntity)})
      }
      .runWith(Sink.head)
      .map{res => system.terminate(); res}

  }

  /**
    * asynchronous (means with async-fold) way to process logs
    *
    * @param source - a  [[akka.stream.scaladsl.Source]] of string log entities
    * @return - [[scala.concurrent.Future]] with a [[de.kaufhof.exercises.logs.model.Model.LogSummary]]
    */
  def processAsync(source: Source[String, NotUsed]): Future[LogSummary] = {

    source
      .map (logEntry => LogEntityParser(logEntry))
      .foldAsync(LogSummary.empty) { case (logSummary, parseResult) =>
        Future {
          logSummary |+| parseResult.fold({_ => LogSummary.failed}, { logEntity => LogSummary(logEntity)})
        }
      }
      .runWith(Sink.head)
      .map{res => system.terminate(); res}

  }
}
