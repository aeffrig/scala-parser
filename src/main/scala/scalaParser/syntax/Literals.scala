package scalaParser
package syntax

import org.parboiled2._

trait Literals {
  self: Parser with Basic with Identifiers with Keywords =>

  def Block: Rule0
  def WL: Rule0

  private final val TQ        = "\"\"\""
  private final val Escapable = "btnfr'\\\""

  object Literals {
    import Basic._
    import Identifiers.{ Id, IdOrKeyword, RawPlainId }

    private def optMinus    = rule( optional('-') )
    private def optExponent = rule( optional(ExponentPart) )
    private def optFloat    = rule( optional(FloatType) )
    private def optLong     = rule( optional(LongType) )

    def pr(s: String) = rule( run(println(s"LOGGING $cursor: $s")) )

    def BlockCommentChar     = rule( MultilineComment | !"*/" ~ ANY )
    def BlockCommentChars    = rule( zeroOrMore(BlockCommentChar) )
    def Comment              = rule( MultilineComment | SingleLineComment )
    def MultilineComment: R0 = rule( "/*" ~ BlockCommentChars ~ "*/" )
    def SingleLineComment    = rule( "//" ~ RestOfLine )

    def BooleanLiteral     = rule( `true` | `false` )
    def CharLiteralChars   = rule( UnicodeEscape | EscapedChar | !'\\' ~ PrintableChar )
    def CharacterLiteral   = rule( ''' ~ CharLiteralChars ~ ''' )
    def EscapedChar        = rule( '\\' ~ anyOf(Escapable) )
    def HexLiteral         = rule( HexNumeral ~ optLong )
    def IntegerLiteral     = rule( IntegerNumeral ~ optLong )
    def MultiLineChars     = rule( zeroOrMore(Interpolation | optional('"') ~ optional('"') ~ noneOf("\"")) )
    def NullLiteral        = rule( `null` )
    def NumericLiteral     = rule( optMinus ~ PositiveLiteral )
    def PositiveLiteral    = rule( FloatingPointLiteral | HexLiteral | IntegerLiteral )
    def SingleInterpolated = rule( Id ~ '"' ~ zeroOrMore(Interpolation | SingleStringChar) ~ '"' )
    def SingleString       = rule( '"' ~ zeroOrMore(SingleStringChar) ~ '"' )
    def SingleStringChar   = rule( "\\\"" | "\\\\" | noneOf("\n\"") )
    def SymbolLiteral      = rule( ''' ~ IdOrKeyword ) // Note that symbols can take on the same values as keywords!
    def TripleInterpolated = rule( Id ~ TripleString )
    def TripleQuoteEnd     = rule( TQ ~ zeroOrMore('"') )
    def TripleQuoteStart   = rule( TQ ~ zeroOrMore(!TQ ~ '"') )
    def TripleString       = rule( TripleQuoteStart ~ MultiLineChars ~ TripleQuoteEnd )

    def FloatingPointLiteral = rule(
        optional(Digits) ~ '.' ~ Digits ~ optExponent ~ optFloat
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
