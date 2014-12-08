package psp
package parser

import org.parboiled2._
import psp.parser.macros.Macros._
import psp.std.{ path, Path }
import psp.std.ansi._

abstract class PspParser extends Parser with Basic with Identifiers with Literals {
  def input: ParserInput

  def prevN(n: Int): String = input.sliceString(cursor - n, cursor)
  def nextN(n: Int): String = input.sliceString(cursor, cursor + n)

  def errorContextWidth: Int            = 1
  def errorCharMarkup(ch: Char): String = {
    import scala.Console._
    RED + BOLD + REVERSED + ch + RESET
  }

  override def formatErrorProblem(error: ParseError): String = "Error"
  override def formatErrorLine(error: ParseError): String = {
    import error._, position._
    def line_s(i: Int): Option[String] = scala.util.Try(
      "%4d  %s".format(i,
        if (i != line) input getLine i
        else input getLine i splitAt column match { case (front, back) =>
          "" + (front dropRight 1) + errorCharMarkup(front.last) + back
        }
      )
    ).toOption
    (line - errorContextWidth) to (line + errorContextWidth) filter (_ >= 1) flatMap line_s mkString "\n"
  }

  def failMessage(path: Path, error: ParseError): String = {
    import error._, position._
    def pos_s = "%s:%s:%s".format(path, line, column)
    s"""|Expected: $formatExpectedAsString
        |  at $pos_s
        |
        |${this formatErrorLine error}
        |""".stripMargin.trim
  }

  /**
   * Parses all whitespace, excluding newlines. This is only
   * really useful in e.g. {} blocks, where we want to avoid
   * capturing newlines so semicolon-inference would work
   */
  def WS = rule( atomic(rep(Basic.WhitespaceChar | Literals.Comment)) )

  /**
   * Parses whitespace, including newlines.
   * This is the default for most things
   */
  def WL = rule( atomic(rep(Basic.WhitespaceChar | Literals.Comment | Basic.Newline)) )
}

class TraceScalaParser(in: ParserInput) extends ScalaParser(in) {
  val counts = scala.collection.mutable.Map[Int, Long]() withDefaultValue 0L
  private val reportFrequency = 100
  private var counted = 0L
  private def tick(): Unit = {
    counted += 1
    counts(cursor) += 1
    if (counted % reportFrequency == 0)
      println(counted / reportFrequency)
  }

  scala.sys addShutdownHook {
    println(s"\nCounted $counted rules.\n")
    counts.toList.sortBy(-_._2) foreach { case (k, v) => println("%6s  %s".format(k, v)) }
  }

  override def Type = rule( super.Type ~ run(tick()) )
}
