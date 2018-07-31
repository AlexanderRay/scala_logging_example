/**
  * model behavior
  *
  * implemented by Alexander Ray (info@alexray.me)
  *
  */
package de.kaufhof.exercises.logs.model

import cats.Monoid
import cats.implicits._
import de.kaufhof.exercises.logs.model.Model.{EntitySummary, LogEntry, LogServerSummary, LogSummary}

/**
  * implicit class or companion object extensions that describes a model behavior
  *
  * the main idea of a log processing is that all of the summary classes a monoids,
  * so that they can be combined
  *
  * it means that logs can be processed in iE following way
  *
  * someLogEntrySource.foldLeft(LogSummary.empty){case (logEntry, logSummary)  =>
  *   LogSummary(logEntry) |+| logSummary
  * }
  *
  */
object ModelBehaviorImplicits {

  /**
    * a entity summary monoid definition
    */
  implicit val entitySummaryMonoid: Monoid[EntitySummary] = new Monoid[EntitySummary] {
    /**
      * zero element
      * @return - returns a empty EntitySummary
      */
    override def empty = EntitySummary(0, 0, 0)

    /**
      * combines two entities
      */
    override def combine(x: EntitySummary, y: EntitySummary): EntitySummary =
      EntitySummary(x.totalMs + y.totalMs, x.count + y.count, x.failed + y.failed)
  }

  /**
    * some extensions of a EntitySummary Companion Object
    *
    */
  implicit class EntitySummaryCompanionOps(val o: EntitySummary.type) extends AnyVal {
    /**
      *
      * @return empty EntitySummary
      */
    def empty: EntitySummary = Monoid[EntitySummary].empty

    /**
      * converts [[de.kaufhof.exercises.logs.model.Model.LogEntry]] to EntitySummary
      * @param le -LogEntry
      * @return - EntitySummary
      */
    def apply(le: LogEntry): EntitySummary =
        EntitySummary(le.duration.ms, 1, if (le.state.failed) 1 else 0)

    /**
      * folds all of the EndPoints summaries from some Server into a aggregated summary
      * @param ls - LogServerSummary
      * @return - aggregated EntitySummary
      */
    def apply(ls: LogServerSummary): EntitySummary =
      ls.es.values.foldLeft(EntitySummary.empty){_ |+| _}
  }


  /**
    * a monoid description of a [[LogServerSummary]]
    */
  implicit val logServerSummaryMonoid: Monoid[LogServerSummary] = new Monoid[LogServerSummary] {

    /**
      * defined a zero element
      */
    override def empty: LogServerSummary = LogServerSummary(Map())

    /**
      * combines two LogServerSummaries
      */
    override def combine(x: LogServerSummary, y: LogServerSummary): LogServerSummary =
      LogServerSummary(
        y.es.toList.foldLeft(x.es) { case (map, (ep, es)) =>
          map + (ep -> (map.getOrElse(ep, EntitySummary.empty) |+| es))
        }
      )
  }

  /**
    * defines some LogServerSummary Companion Object extensions
    */
  implicit class LogServerSummaryCompanionOps(val o: LogServerSummary.type) extends AnyVal {
    /**
      * @return - empty LogServerSummary
      */
    def empty: LogServerSummary = Monoid[LogServerSummary].empty

    /**
      * converts LogEntry into LogServerSummary
      * @param le - LogEntry
      * @return - LogServerSummary
      */
    def apply(le: LogEntry): LogServerSummary = LogServerSummary(Map(le.url.endPoint -> EntitySummary(le)))
  }

  /**
    * a monoid description of a LogSummary
    */
  implicit val logSummaryMonoid: Monoid[LogSummary] = new Monoid[LogSummary] {
    /**
      * defines zero element
      */
    override def empty = LogSummary(EntitySummary.empty, Map())

    /**
      * combines two LogSummary
      */
    override def combine(x: LogSummary, y: LogSummary) =
      LogSummary(
        x.processSummary |+| y.processSummary,
        y.serverSummary.toList.foldLeft(x.serverSummary) { case (map, (s, lss)) =>
          map + (s -> (map.getOrElse(s, LogServerSummary.empty) |+| lss))
        }
      )
  }


  /**
    * defines some LogSummary Companion Object extensions
    */
  implicit class LogSummaryCompanionOps(val o: LogSummary.type) extends AnyVal {
    /**
      * returns an empty LogSummary
      */
    def empty: LogSummary = Monoid[LogSummary].empty

    /**
      * converts a LogEntry into LogSummary
      */
    def apply(le: LogEntry): LogSummary =
      LogSummary(EntitySummary(0, 1, 0), Map(le.url.server -> LogServerSummary(le)))

    /**
      * creates a failed LogSummary (failed == some LexerError occurred)
      */
    def failed: LogSummary = LogSummary(EntitySummary(0, 1, 1), Map())
  }

}
