package scalaParser
package syntax

import org.parboiled2._
import macros.Macros._

trait Literals {
  self: PspParser =>

  def Block: Rule0
  def WL: Rule0

  private final val TQ        = "\"\"\""
  private final val Escapable = "0btnfr'\\\""

  object Literals {
    import Basic._
    import Identifiers.{ Id, IdOrKeyword, RawPlainId }

    private def optMinus    = rule( opt('-') )
    private def optExponent = rule( opt(ExponentPart) )
    private def optFloat    = rule( opt(FloatType) )
    private def optLong     = rule( opt(LongType) )

    private def literal(s: String) = rule( WL ~ str(s) ~ !Basic.AlphaNum )

    def BlockCommentBegin = rule( "/*" )
    def BlockCommentEnd   = rule( "*/" )

    def `true`  = rule( literal("true") )
    def `false` = rule( literal("false") )
    def `null`  = rule( literal("null") )

    def BlockCommentChar     = rule( MultilineComment | !BlockCommentEnd ~ ANY )
    def BlockCommentChars    = rule( rep(BlockCommentChar) )
    def Comment              = rule( MultilineComment | SingleLineComment )
    def MultilineComment: R0 = rule( BlockCommentBegin ~ BlockCommentChars ~ BlockCommentEnd )
    def SingleLineComment    = rule( "//" ~ RestOfLine )

    def BooleanLiteral     = rule( `true` | `false` )
    def CharLiteralChars   = rule( UnicodeEscape | OctalEscape | EscapedChar | !'\\' ~ PrintableChar )
    def CharacterLiteral   = rule( ''' ~ CharLiteralChars ~ ''' )
    def EscapedChar        = rule( '\\' ~ anyOf(Escapable) )
    def HexLiteral         = rule( HexNumeral ~ optLong )
    def IntegerLiteral     = rule( IntegerNumeral ~ optLong )
    def MultiLineChars     = rule( rep(Interpolation | opt('"') ~ opt('"') ~ noneOf("\"")) )
    def NullLiteral        = rule( `null` )
    def NumericLiteral     = rule( optMinus ~ PositiveLiteral )
    def PositiveLiteral    = rule( FloatingPointLiteral | HexLiteral | IntegerLiteral )
    def SingleInterpolated = rule( Id ~ '"' ~ rep(Interpolation | SingleStringChar) ~ '"' )
    def SingleString       = rule( '"' ~ rep(SingleStringChar) ~ '"' )
    def SingleStringChar   = rule( "\\\"" | "\\\\" | noneOf("\n\"") )
    def SymbolLiteral      = rule( ''' ~ IdOrKeyword ) // Note that symbols can take on the same values as keywords!
    def TripleInterpolated = rule( Id ~ TripleString )
    def TripleQuoteEnd     = rule( TQ ~ rep('"') )
    def TripleQuoteStart   = rule( TQ ~ rep(!TQ ~ '"') )
    def TripleString       = rule( TripleQuoteStart ~ MultiLineChars ~ TripleQuoteEnd )

    def FloatingPointLiteral = rule(
        opt(Digits) ~ '.' ~ Digits ~ optExponent ~ optFloat
      | Digits ~ ExponentPart ~ optFloat
      | Digits ~ optExponent ~ FloatType
    )
    def Literal = rule(
        NumericLiteral
      | BooleanLiteral
      | CharacterLiteral
      | StringLiteral
      | SymbolLiteral
      | NullLiteral
    )
    def StringLiteral = rule(
        TripleInterpolated
      | SingleInterpolated
      | TripleString
      | SingleString
    )
    def Interpolation = rule(
        "$" ~ RawPlainId
      | "${" ~ Block ~ WL ~ "}"
      | "$$"
    )
  }
}
