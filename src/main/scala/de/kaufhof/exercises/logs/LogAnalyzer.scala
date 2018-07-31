/**
  *
  * Log Analyzer
  *
  */
package de.kaufhof.exercises.logs

import akka.stream.scaladsl.Source
import processor.LogProcessor
import model.ModelTypeClasses._
import model.ModelStringPrinterImplicits._

import scala.concurrent.ExecutionContext.Implicits.global

object LogAnalyzer {
  /**
    *
    * main function,
    * 1) creates an iterator of log entities,
    * 2) processes it with a LogProcessor
    * 3) print a summary result
    *
    */
  def main(args: Array[String]): Unit = {

    val src = new LogProducer(1000000)
    val startTime = System.nanoTime

    LogProcessor().process(Source.fromIterator(() => src.logs))
      .map(ls => ls.print)
      .foreach{ res =>
        println(res)
        printf("Elapsed: %.3f s\n",1e-9*(System.nanoTime-startTime))
      }
  }

  /**
    *
    * creates an iterator with string log entities
    *
    * @param count - count of a log entities
    */
  class LogProducer(count: Int) {
    import java.text.SimpleDateFormat
    import java.util.Date

    import Internal._

    import scala.util.Random

    val logs = Iterator.range(0, count).map { i =>
      val level = oneOf(levels)
      val msg = if (isError(level))
        s"error while fetching $url after $took"
      else s"fetch $url took: $took"

      s"${formatter.format(new Date())} $level - $msg"
    }

    private object Internal {
      val formatter = new SimpleDateFormat("HH:mm:ss:SS")
      val levels = "INFO" :: "INFO" :: "INFO" :: "INFO" :: "WARN" :: Nil
      val endpoints = "teaser" :: "profile" :: "products" :: Nil
      val servers = "srv1.kaufhof.de" :: "srv2.kaufhof.de" :: Nil

      def isError(level: String) = level == "WARN"

      def oneOf[T](xs: List[T]) = xs(Random.nextInt(xs.size))
      def took = s"${20 + Random.nextInt(400)} ms"
      def url = s"http://${oneOf(servers)}/${oneOf(endpoints)}.json"
    }
  }
}