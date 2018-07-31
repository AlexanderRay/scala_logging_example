/**
  * implemented by Alexander Ray (info@alexray.me)
  *
  * Log Parser
  */
package de.kaufhof.exercises.logs.parser

import de.kaufhof.exercises.logs.model.Model
import de.kaufhof.exercises.logs.model.Model._

import scala.util.parsing.combinator.RegexParsers

/**
  * A parser object.
  *
  * It parse string Log representation und converts it to [[de.kaufhof.exercises.logs.model.Model]]
  * or creates a LexerError
  *
  * it assumes that log entry has a following format
  *
  * 'HH:mm:ss:SS INFO - fetch http://srv{X}.kaufhof.de/endpoint.json took: timeInMilliseconds ms'
  *
  * or
  *
  * 'HH:mm:ss:SS WARN - error while fetching http://srv{X}.kaufhof.de/endpoint.jso after timeInMilliseconds ms'
  *
  * the endpoints are 'teaser', 'profile', 'products'
  *
  */
object LogEntityParser extends RegexParsers {

  def failure   = "WARN"                  ^^ (_ => LogFailed )
  def success   = "INFO"                  ^^ ( _ => LogSuccess )
  def ms        = "ms"                    ^^ ( _.toString )
  def teaser    = "teaser"                ^^ ( _ => Model.Teaser )
  def profile   = "profile"               ^^ ( _ => Model.Profile )
  def products  = "products"              ^^ ( _ => Model.Products )

  def number    = """([0-9]\d*)""".r                              ^^ { _.toInt }
  def duration  = ("took:" | "after") ~> number <~ ms             ^^ ( n => Duration(n) )

  def server    = "srv" ~> """([0-9])""".r <~ ".kaufhof.de" ^^ { id => Server(id.toInt) }
  def endPoint  = (teaser | profile | products) ~ ".json"   ^^ { _._1 }
  def url       =  "http://" ~ server ~ "/"  ~  endPoint    ^^ { p =>
    Url(p._1._1._2, p._2)
  }

  def timeNumber = """([0-9]\d*)""".r  ^^ {_.toInt}
  def timePrefix = timeNumber ~ ":" ~ timeNumber ~ ":" ~ timeNumber ~ ":" ~ timeNumber
  def logInfoText = "error while fetching" | "fetch" ^^ ( _.toString )

  def logEntry  = timePrefix ~ (failure | success) ~ "-" ~ logInfoText ~ url ~ duration ^^ { p =>
      LogEntry(p._1._1._1._1._2, p._1._2, p._2)
  }

  /**
    *
    * @param data - a string log representation
    *
    * @return -  [[de.kaufhof.exercises.logs.model.Model.LogEntry]]
    *         or [[de.kaufhof.exercises.logs.model.Model.LexerError]]
    */
  def apply(data: String): Either[LexerError, LogEntry] = {
    parse(logEntry, data) match {
      case Success(le, _)  => Right(le)
      case NoSuccess(msg, _) => Left(LexerError(msg))
    }
  }
}
