package scalaParser
package syntax

import org.parboiled2._
import CharPredicate.{ HexDigit, AlphaNum, Digit19 }
import scalaParser.macros.Macros._
import java.lang.Integer.parseInt
import java.lang.{ Character => C }, C.UnicodeBlock

trait Basic {
  self: PspParser =>

  type R0    = Rule0
  type R1[T] = Rule1[T]
  type F0R0  = () => Rule0

  private final val AsciiOpChars     = "\\!#$%&*+-:<=>?@^|~"
  private final val UnicodeStart     = "\\u"
  private final val LineCommentStart = "//"

  object Basic {
    private def prevCodePoint = CodePoint(prevN(4))

    def UnicodeEscape: R0        = rule( UnicodeStart ~ (4 times HexDigit) )
    def UnicodeOperator: R0      = rule( UnicodeEscape ~ test(prevCodePoint.isOperator) )
    def UnicodeLetter: R0        = rule( UnicodeEscape ~ test(prevCodePoint.isLetter) )
    def UnicodeUpper: R0         = rule( UnicodeEscape ~ test(prevCodePoint.isUpper) )
    def UnicodeLower: R0         = rule( UnicodeEscape ~ test(prevCodePoint.isLower) )
    def UnicodeDigit: R0         = rule( UnicodeEscape ~ test(prevCodePoint.isDigit) )
    def UnicodePrintableChar: R0 = rule( UnicodeEscape ~ test(prevCodePoint.isPrintable) )

    def OperatorChar: R0 = rule(
        UnicodeOperator
      | anyOf(AsciiOpChars)
      | !LineCommentStart ~ ( '/' | isOperator )
    )

    def AlphaNum       = rule( Letter | Digit )
    def DelimiterChar  = rule( atomic(anyOf("'\".;,")) )
    def Digit          = rule( atomic(CharPredicate.Digit | UnicodeDigit) )
    def Digits         = rule( oneOrMore(Digit) )
    def ExponentPart   = rule( anyOf("Ee") ~ optional(anyOf("+-")) ~ oneOrMore(Digit) )
    def FloatType      = rule( anyOf("FfDd") )
    def HexNumeral     = rule( "0x" ~ oneOrMore(HexDigit) )
    def IntegerNumeral = rule( Digit19 ~ zeroOrMore(Digit) | "0" ~ !Digit )
    def Letter         = rule( atomic(Upper | Lower | UnicodeLetter) )
    def LongType       = rule( anyOf("Ll") )
    def Lower          = rule( "a" - "z" | "$" | "_" | IsLower | UnicodeLower )
    def Newline        = rule( atomic("\r\n" | "\n") )
    def NewlineOrEnd   = rule( Newline | EOI )
    def OctalEscape    = rule( "\\" ~ Digit ~ opt(Digit) ~ opt(Digit) )
    def Parentheses    = rule( anyOf("()[]{}") )
    def PrintableChar  = rule( (CharPredicate from (c => CodePoint(c).isPrintable)) | UnicodePrintableChar )
    def RestOfLine     = rule( rep(!Newline ~ ANY) ~ &(NewlineOrEnd) )
    def Semi           = rule( atomic( ';' | rep1(Newline) ) )
    def Upper          = rule( atomic("A" - "Z" | IsUpper | UnicodeUpper) )
    def WhitespaceChar = rule( anyOf(" \t") )

    private def IsAlpha: R0    = rule( CharPredicate from (_.isLetter) )
    private def IsLower: R0    = rule( CharPredicate from (_.isLower) )
    private def IsUpper: R0    = rule( CharPredicate from (_.isUpper) )
    private def isOperator: R0 = rule( CharPredicate from (c => OperatorTypes(c.getType)) )

    private val OperatorTypes = Set[Int](Character.OTHER_SYMBOL, Character.MATH_SYMBOL)
  }
}
