import org.parboiled2._
import psp.std._, ansi._
import psp.parser._

package object repl {
  def csvParse(in: ParserInput) = CsvParser(in).parseAll()
  def parse(in: ParserInput)    = newScalaParser(in).parseAll()
}
