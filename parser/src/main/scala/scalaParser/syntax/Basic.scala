package scalaParser
package syntax

import org.parboiled2._
import CharPredicate.{ HexDigit, AlphaNum, Digit19 }
import scalaParser.macros.Macros._

trait Basic {
  self: Parser =>

  type R0 = Rule0

  object Basic {
    def AlphaNum       = rule( Letter | Digit )
    def DelimiterChar  = rule( anyOf("'\".;,") )
    def Digit          = CharPredicate.Digit
    def Digits         = rule( oneOrMore(Digit) )
    def ExponentPart   = rule( anyOf("Ee") ~ optional(anyOf("+-")) ~ oneOrMore(Digit) )
    def FloatType      = rule( anyOf("FfDd") )
    def HexNumeral     = rule( "0x" ~ oneOrMore(HexDigit) )
    def IntegerNumeral = rule( Digit19 ~ zeroOrMore(Digit) | "0" ~ !Digit )
    def Letter         = rule( Upper | Lower | IsAlpha )
    def LongType       = rule( anyOf("Ll") )
    def Lower          = rule( "a" - "z" | "$" | "_" | IsLower )
    def Newline        = rule( "\r\n" | "\n" )
    def NewlineOrEnd   = rule( Newline | EOI )
    def OctalEscape    = rule( "\\" ~ Digit ~ opt(Digit) ~ opt(Digit) )
    def OperatorChar   = rule( anyOf("\\!#$%&*+-/:<=>?@^|~") | isOperator )
    def Parentheses    = rule( anyOf("()[]{}") )
    def PrintableChar  = CharPredicate from isPrintableChar
    def RestOfLine     = rule( zeroOrMore(!Newline ~ ANY) ~ &(NewlineOrEnd) )
    def Semi           = rule( ';' | oneOrMore(Newline) )
    def UnicodeEscape  = rule( "\\u" ~ 4.times(HexDigit) )
    def Upper          = rule( "A" - "Z" | IsUpper )
    def WhitespaceChar = rule( "\u0020" | "\u0009" )

    def isPrintableChar(c: Char): Boolean = {
      import java.lang.Character._
      UnicodeBlock of c match {
        case null | UnicodeBlock.SPECIALS => false
        case _                            => !isISOControl(c) && !isSurrogate(c)
      }
    }

    private def IsAlpha    = CharPredicate from (_.isLetter)
    private def IsLower    = CharPredicate from (_.isLower)
    private def IsUpper    = CharPredicate from (_.isUpper)
    private def isOperator = CharPredicate from (c => OperatorTypes(c.getType))

    private val OperatorTypes = Set[Int](Character.OTHER_SYMBOL, Character.MATH_SYMBOL)
  }
}
