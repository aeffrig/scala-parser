package scalaParser
package syntax
import org.parboiled2._
import CharPredicate.{ HexDigit, AlphaNum, Digit, Digit19 }

trait Basic {
  self: Parser =>

  implicit class RuleOps[I <: HList, O <: I](r: Rule[I, O]) {
    def * = rule( zeroOrMore(r) )
    def + = rule( oneOrMore(r) )
  }

  object Basic {
    def Digit          = CharPredicate.Digit
    def DecimalNumeral = rule( oneOrMore(Digit) )
    def DelimiterChar  = rule( anyOf("'\".;,") )
    def ExponentPart   = rule( anyOf("Ee") ~ optional(anyOf("+-")) ~ oneOrMore(Digit) )
    def FloatType      = rule( anyOf("FfDd") )
    def HexNumeral     = rule( "0x" ~ oneOrMore(HexDigit) )
    def Letter         = rule( Upper | Lower | IsAlpha | Digit )
    def Lower          = rule( "a" - "z" | "$" | "_" | IsLower )
    def Newline        = rule( "\r\n" | "\n" )
    def OperatorChar   = rule( anyOf("\\!#$%&*+-/:<=>?@^|~") | isOperator )
    def Parentheses    = rule( anyOf("()[]{}") )
    def Semi           = rule( ';' | oneOrMore(Newline) )
    def UnicodeExcape  = rule( "\\u" ~ 4.times(HexDigit) )
    def Upper          = rule( "A" - "Z" | IsUpper )
    def WhitespaceChar = rule( "\u0020" | "\u0009" )

    private def IsAlpha    = CharPredicate from (_.isLetter)
    private def IsLower    = CharPredicate from (_.isLower)
    private def IsUpper    = CharPredicate from (_.isUpper)
    private def isOperator = CharPredicate from (c => OperatorTypes(c.getType))

    private val OperatorTypes = Set[Int](Character.OTHER_SYMBOL, Character.MATH_SYMBOL)
  }
  /**
   * Most keywords don't just require the correct characters to match,
   * they have to ensure that subsequent characters *don't* match in
   * order for it to be a keyword. This enforces that rule for key-words
   * (W) and key-operators (O) which have different non-match criteria.
   */
  object Key {
    def W(s: String) = rule {
      str(s) ~ !Basic.Letter
    }

    def O(s: String) = rule {
      str(s) ~ !Basic.OperatorChar
    }
  }
}
