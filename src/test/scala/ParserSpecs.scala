/**
  * implemented by Alexander Ray (info@alexray.me)
  */

import de.kaufhof.exercises.logs.model.Model._
import de.kaufhof.exercises.logs.parser.LogEntityParser
import org.scalatest.{FlatSpec, Matchers}

class ParserSpecs extends FlatSpec with Matchers {


  "logState parser" should "recognize success" in {
    val successString = "INFO"

    val res = LogEntityParser.parse(LogEntityParser.success, successString)
    res.get should be(LogSuccess)
  }

  it should "recognize failure" in {
    val failedString  = "WARN"

    val res = LogEntityParser.parse(LogEntityParser.failure, failedString)
    res.get should be(LogFailed)
  }

  it should "recognize failure info text" in {
    val failedString  = "error while fetching"

    val res = LogEntityParser.parse(LogEntityParser.logInfoText, failedString)
    res.get should be(failedString)
  }

  "logDuration parser" should "recognize 320 ms duration" in {
    val durationString = "after 320 ms"

    val res = LogEntityParser.parse(LogEntityParser.duration, durationString)
    res.get should be (Duration(320))
  }

  "logServer parser" should "recognize srv1.kaufhof.de server" in {
    val serverString = "srv1.kaufhof.de"

    val res = LogEntityParser.parse(LogEntityParser.server, serverString)
    res.get should be (Server(1))

  }

  "logEndpoint parser" should "recognize 'profile' endpoint" in {
    val endPointString = "profile.json"

    val res = LogEntityParser.parse(LogEntityParser.endPoint, endPointString)
    res.get should be (Profile)
  }

  "logUrl parser" should "recognize 'http://srv2.kaufhof.de/teaser.json' url" in {
    val serverUrl = "http://srv2.kaufhof.de/teaser.json"

    val res = LogEntityParser.parse(LogEntityParser.url, serverUrl)
    res.get should be(Url(Server(2), Teaser))
  }

  "logEntry parser" should "recognize some log entry" in {
    val logEntryString = "21:23:12:66 WARN - error while fetching http://srv1.kaufhof.de/profile.json after 403 ms"

    val res = LogEntityParser.parse(LogEntityParser.logEntry, logEntryString)
    res.get should be(LogEntry(LogFailed, Url(Server(1), Profile), Duration(403)))
  }

  it should "recognize some incorrect log entry" in {
    val logEntryString = "21:23:12:66 WARN - error while fetching http://srv1.kaufhof.de/profile.jso after 403 ms"

    val res = LogEntityParser(logEntryString)

    res.left.get should be(LexerError("`.json' expected but `.' found"))
  }

}
