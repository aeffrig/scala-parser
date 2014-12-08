package psp

import org.parboiled2._
import psp.std.{ path, Path }
import scala.sys.process.Process

package object parser {
  val any2stringadd = null

  type HList = shapeless.HList

  def doto[A](value: A)(f: A => Unit): A = { f(value) ; value }

  def abort(msg: String) = sys error msg

  def paths(root: String): Seq[Path] = paths(path(root))
  def paths(root: Path): Seq[Path]   = (
    if (root.toFile.isFile)
      Seq(root)
    else
      Process(Seq("find", s"$root/", "-type", "f", "-name", "*.scala", "-print")).lines map (x => path(x))
  )

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
