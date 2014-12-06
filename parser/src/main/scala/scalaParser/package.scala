import org.parboiled2._

package object scalaParser {
  val any2stringadd = null

  type HList = shapeless.HList

  def doto[A](value: A)(f: A => Unit): A = { f(value) ; value }

  def abort(msg: String) = sys error msg

  /* Ugh. */
  def toCamelCase(s: String): String = {
    val s1 = if (s == "") "" else "" + s.head + s.tail.map(_.toLower)
    val m = java.util.regex.Pattern compile "_([a-z])" matcher s1
    val sb = new StringBuffer
    while (m.find()) m.appendReplacement(sb, m group 1 toUpperCase)
    m appendTail sb
    sb.toString
  }

  def newSyntax(in: ParserInput): ScalaSyntax =
    if (sys.props contains "debug") new TraceScalaSyntax(in)
    else new ScalaSyntax(in)
}
