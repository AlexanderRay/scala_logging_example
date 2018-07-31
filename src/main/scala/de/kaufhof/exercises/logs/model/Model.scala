/**
  * entity and summary log models
  *
  * implemented by Alexander Ray (info@alexray.me)
  *
  */
package de.kaufhof.exercises.logs.model

object Model {

  /**
    * model root trait
    */
  sealed trait Root

  /**
    * logs root trait
    */
  sealed trait LogEntity extends Root

  /**
    * end points: teaser, profile or products endpoints
    */
  sealed trait EndPoint extends LogEntity
  case object Teaser extends EndPoint
  case object Profile extends EndPoint
  case object Products extends EndPoint

  /**
    * Server description
    * @param id - server id
    */
  case class Server(id: Int) extends LogEntity

  /**
    * url - representation
    *
    * @param server - server
    * @param endPoint - endpoint
    */
  case class Url(server: Server, endPoint: EndPoint) extends LogEntity

  /**
    * log state: success or failure log states
    */
  sealed trait LogState extends LogEntity {def failed: Boolean}
  case object LogSuccess extends LogState {val failed = false}
  case object LogFailed extends LogState {val failed = true}

  /**
    * Duration
    * @param ms - duration in milliseconds
    */
  case class Duration(ms: Int) extends LogEntity

  /**
    * typed LogEntry
    * @param state - [[LogState]]
    * @param url - [[Url]]
    * @param duration - [[Duration]]
    *
    * it is a typed representation of a log in following format
    *
    * 'HH:mm:ss:SS INFO - fetch http://srv{X}.kaufhof.de/endpoint.json took: timeInMilliseconds ms'
    *
    * or
    *
    * 'HH:mm:ss:SS WARN - error while fetching http://srv{X}.kaufhof.de/endpoint.jso after timeInMilliseconds ms'
    *
    */
  case class LogEntry(state: LogState, url: Url, duration: Duration) extends LogEntity

  /**
    * Parser Errors
    */
  sealed trait ParserError extends Root

  /**
    * a lexer error
    * @param msg - error message
    */
  case class LexerError(msg: String) extends ParserError

  /**
    * summary model root
    */
  sealed trait Summary extends Root

  /**
    * summary entity
    * @param totalMs - sum of durations
    * @param count - count of aggregated log entries
    * @param failed - count of failed logs (parser failures or log level failures)
    */
  case class EntitySummary(totalMs: BigInt, count: Int, failed: Int) extends Summary {
    /**
      * average duration
      */
    lazy val avgMs: BigInt = if (count==0) 0 else totalMs / count
    /**
      * percent of successed entries
      */
    lazy val successP: Int = if (count==0) 0 else 100 * (count - failed) / count
  }

  /**
    * a summary per end point
    *
    * @param es - a map endpoint to summary
    */
  case class LogServerSummary(es: Map[EndPoint, EntitySummary]) extends Summary

  /**
    * a complete log summary
    *
    * @param processSummary - processed summary with count of parse (= lexer) failures
    * @param serverSummary - a summary of endpoints per server
    */
  case class LogSummary(processSummary: EntitySummary, serverSummary: Map[Server, LogServerSummary]) extends Summary

}
