package psp

import org.parboiled2._
import psp.std._, api._, ansi._, pio._
import scala.sys.process.Process

package object parser {
  type HList = shapeless.HList

  def paths(root: String): Each[Path] = paths(path(root))
  def paths(root: Path): Each[Path]   = root.entries flatMap (_.deepFiles) filter (_ hasExtension "scala")

  implicit def pathToParserInput(p: Path): ParserInput  = p.slurp()
  implicit def fileToParserInput(f: jFile): ParserInput = f.toPath.slurp()

  def newScalaParser(in: ParserInput): ScalaParser =
    if (sys.props contains "debug") new TraceScalaParser(in)
    else new ScalaParser(in)
}

package parser {
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
    def expected = err.formatExpectedAsString
    def isEmpty  = true
    def get      = abort(expected)
    def pos      = err.position
    def index    = pos.index
    def ch       = (in charAt index).toString
    def before   = slice(0, index)
    def after    = slice(index + 1, length)

    def slice(start: Int, end: Int) = in.sliceString(start, end)
    override def toString = Seq(s"Expected $expected", s"  at $coords", "", before + ch.toString.red.to_s + after) mkString "\n"
  }
}
