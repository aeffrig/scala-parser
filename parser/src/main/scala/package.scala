package psp

import org.parboiled2._
import psp.std._, api._, ansi._, pio._, StdShow._
import scala.sys.process.Process

package object parser {
  type HList = shapeless.HList

  def paths(root: String): Each[Path] = paths(path(root))
  def paths(root: Path): Each[Path]   = root.entries flatMap (_.deepFiles) filter (_ hasExtension "scala")

  implicit def pathToParserInput(p: Path): ParserInput  = p.slurp()
  implicit def fileToParserInput(f: jFile): ParserInput = f.toPath.slurp()

  def failMessage(path: Path, error: ParseError, input: ParserInput): String = {
    val err = PspError(input, error)
    import err._
    s"""|Expected: $expected
        |  at $path:$line$column
        |
        |$formattedLine
        |""".stripMargin.trim
  }

  def isDebug = sys.props contains "debug"
  def newScalaParser(in: ParserInput): ScalaParser =
    if (isDebug) new TraceScalaParser(in) else new ScalaParser(in)
}

package parser {
  import scala.Console._

  class PspErrorFormatter extends ErrorFormatter {
    def errorContextWidth: Int            = 1
    def errorCharMarkup(ch: Char): String = RED + BOLD + REVERSED + ch + RESET

    override def formatErrorLine(error: ParseError, input: ParserInput): String = {
      import error._, position._

      def line_s(i: Int): Each[String] = scala.util.Try(
        "%4d  %s".format(i,
          if (i != line) input getLine i
          else input getLine i splitAt Index(column) match { case Split(front, back) =>
            "" + (front dropRight 1) + errorCharMarkup(front.last) + back
          }
        )
      ).toOption.seq

      (line - errorContextWidth) to (line + errorContextWidth) filter (_ >= 1) flatMap line_s mk_s "\n"
    }
    // override def format(error: ParseError, input: ParserInput): String
    // override def format(sb: JStringBuilder, error: ParseError, input: ParserInput): JStringBuilder
    // override def formatProblem(error: ParseError, input: ParserInput): String
    // override def formatProblem(sb: JStringBuilder, error: ParseError, input: ParserInput): JStringBuilder
    // override def formatExpected(error: ParseError): String
    // override def formatExpected(sb: JStringBuilder, error: ParseError): JStringBuilder
    // override def formatExpectedAsString(error: ParseError): String
    // override def formatExpectedAsString(sb: JStringBuilder, error: ParseError): JStringBuilder
    // override def formatExpectedAsList(error: ParseError): List[String]
    // override def formatAsExpected(trace: RuleTrace): String
    // override def formatErrorLine(error: ParseError, input: ParserInput): String
    // override def formatErrorLine(sb: JStringBuilder, error: ParseError, input: ParserInput): JStringBuilder
    // override def expandErrorLineTabs(line: String, errorColumn: Int): (Int, String)
    // override def formatTraces(error: ParseError): String
    // override def formatTrace(trace: RuleTrace, errorIndex: Int): String
    // override def formatTraceHead(trace: RuleTrace, showFrameStartOffset: Boolean): String
  }

  sealed trait PspParsed[+A] {
    def in: ParserInput
    def isEmpty: Boolean
    def get: A
    def pos: Position

    def isSuccess = !isEmpty
    def line      = pos.line
    def column    = pos.column
    def coords    = s"$line:$column"
    def length    = in.length
    def in_s      = in.sliceString(0, length)
  }
  final case class PspResult[A](in: ParserInput, pos: Position, value: A) extends PspParsed[A] {
    def isEmpty = false
    def get     = value
    override def toString = s"""
      |Result(
      |  $in_s
      |)""".trim.stripMargin
  }
  final case class PspError(in: ParserInput, err: ParseError) extends PspParsed[Nothing] {
    val fmt = new PspErrorFormatter

    def isInRange     = 0 <= index && index < in.length
    def after         = slice(index + 1, length)
    def before        = slice(0, index)
    def ch            = if (isInRange) (in charAt index).toString else ""
    def columnNo      = pos.column
    def expected      = fmt formatExpectedAsString err
    def formattedLine = fmt.formatErrorLine(err, in)
    def get           = abort(expected)
    def index         = pos.index
    def isEmpty       = true
    def lineNo        = pos.line
    def pos           = err.position

    def slice(start: Int, end: Int): String = (
      if (end <= 0 || end <= start) ""
      else in.sliceString(start max 0, end min in.length)
    )
    override def toString = Seq(s"Expected $expected", s"  at $coords", "", before + ch.toString.red.to_s + after) mkString "\n"
  }
}
