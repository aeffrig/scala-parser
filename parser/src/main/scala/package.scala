package psp

import org.parboiled2._
import psp.std._, ansi._
import scala.sys.process.Process

package object parser {
  val any2stringadd = null

  type HList = shapeless.HList

  def resourcesIn(root: String): Vector[(String, String)] = (
    Resource.getResourceListing(getClass, root + "/").toVector
      map (name => (name, resourceString(s"$root/$name")))
  )

  def doto[A](value: A)(f: A => Unit): A = { f(value) ; value }

  def abort(msg: String) = sys error msg

  def paths(root: String): Seq[Path] = paths(path(root))
  def paths(root: Path): Seq[Path]   = (
    if (root.toFile.isFile)
      Seq(root)
    else
      Process(Seq("find", "-L", s"$root/", "-type", "f", "-name", "*.scala", "-print")).lines map (x => path(x))
  )

  implicit def pathToParserInput(p: Path): ParserInput  = p.slurp()
  implicit def fileToParserInput(f: jFile): ParserInput = f.toPath.slurp()

  /* Ugh. */
  def toCamelCase(s: String): String = {
    val s1 = if (s == "") "" else "" + s.head + s.tail.map(_.toLower)
    val m = java.util.regex.Pattern compile "_([a-z])" matcher s1
    val sb = new StringBuffer
    while (m.find()) m.appendReplacement(sb, m group 1 toUpperCase)
    m appendTail sb
    sb.toString
  }
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
