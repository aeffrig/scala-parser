package psp
package parser

import org.parboiled2._
import psp.parser.macros.Macros._

object CsvParser {
  final case class CsvFile(records: Seq[Record])
  final case class Record(fields: Seq[String])
  final case class Error(msg: String)

  def apply(input: ParserInput): CsvParser = new CsvParser(input, ',')
}

/**
 * Simple, fast CSV parser.
 *
 * See http://tools.ietf.org/html/rfc4180#section-2
 */
class CsvParser(val input: ParserInput, delimiter: Char) extends HasInputParser with StringBuilding {
  import CsvParser._

  val CharBase     = CharPredicate.Printable -- '"'
  val UnquotedChar = CharBase -- delimiter

  def startRule     = rule( records ~> ((x: Any) => ()) )

  def QuotedChar    = rule( CharBase | NL | '"' ~ '"' )
  def records       = rule( OWS ~ rep1sep(fields ~> Record, NL) ~ opt(NL) )
  def file          = rule( records ~ EOI ~> CsvFile )
  def fields        = rule( rep1sep(quotedField | unquotedField,  delimiter ~ OWS) )
  def quotedField   = rule( '"' ~ clearSB() ~ rep(QuotedChar ~ appendSB()) ~ '"' ~ OWS ~ push(sb.toString) )
  def unquotedField = rule( capture(rep(UnquotedChar)) )
  def NL            = rule( opt('\r') ~ '\n' ~ OWS )
  def OWS           = rule( rep(' ') )
}
