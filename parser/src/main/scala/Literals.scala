package psp
package parser

import org.parboiled2._
import macros.Macros._

trait Literals {
  self: PspParser =>

  def CasePattern: Rule0
  def ImpliedBlock: Rule0
  def WL: Rule0

  private final val TQ        = "\"\"\""
  private final val Escapable = "0btnfr'\\\""

  object Literals {
    import Basic._
    import Identifiers.{ Id, IdOrKeyword }

    private def optMinus    = rule( opt('-') )
    private def optExponent = rule( opt(ExponentPart) )
    private def optFloat    = rule( opt(FloatType) )
    private def optLong     = rule( opt(LongType) )

    private def literal(s: String) = rule( WL ~ str(s) ~ !Basic.AlphaNum )

    def BlockCommentBegin = rule( "/*" )
    def BlockCommentEnd   = rule( "*/" )
    def Backslash         = rule( '\\' )
    def Quote             = rule( '"' )
    def OptQuote          = rule( opt(Quote) )
    def NonQuoteChar      = rule( !Quote ~ ANY )
    def SingleChar        = rule( "\\\"" | "\\\\" | noneOf("\n\"") )
    def TripleChar        = rule( OptQuote ~ OptQuote ~ NonQuoteChar )

    def `true`  = rule( literal("true") )
    def `false` = rule( literal("false") )
    def `null`  = rule( literal("null") )

    def BlockCommentChar     = rule( MultilineComment | !BlockCommentEnd ~ ANY )
    def BlockCommentChars    = rule( rep(BlockCommentChar) )
    def Comment              = rule( MultilineComment | SingleLineComment )
    def MultilineComment: R0 = rule( BlockCommentBegin ~ BlockCommentChars ~ BlockCommentEnd )
    def SingleLineComment    = rule( "//" ~ RestOfLine )

    def BooleanLiteral   = rule( `true` | `false` )
    def CharLiteralChars = rule( UnicodeEscape | OctalEscape | EscapedChar | noneOf("\\") )
    def CharacterLiteral = rule( ''' ~ CharLiteralChars ~ ''' )
    def EscapedChar      = rule( Backslash ~ anyOf(Escapable) )
    def HexLiteral       = rule( HexNumeral ~ optLong )
    def IntegerLiteral   = rule( IntegerNumeral ~ optLong )
    def NullLiteral      = rule( `null` )
    def NumericLiteral   = rule( optMinus ~ PositiveLiteral )
    def PositiveLiteral  = rule( FloatingPointLiteral | HexLiteral | IntegerLiteral )
    def SingleString     = rule( Quote ~ rep(SingleChar) ~ Quote )
    def SymbolLiteral    = rule( ''' ~ IdOrKeyword ) // Note that symbols can take on the same values as keywords!
    def TripleEnd        = rule( TQ ~ rep('"') )
    def TripleStart      = rule( TQ ~ rep(!TQ ~ '"') )
    def TripleString     = rule( TripleStart ~ rep(TripleChar) ~ TripleEnd )

    def FloatingPointLiteral = rule(
        opt(Digits) ~ '.' ~ Digits ~ optExponent ~ optFloat
      | Digits ~ ExponentPart ~ optFloat
      | Digits ~ optExponent ~ FloatType
    )

    abstract class PatternSensitive {
      def inPattern: Boolean

      private def InterpId          = rule( "$" ~ Identifiers.RawPlainId )
      private def SingleChars       = rule( rep(Interpolation | SingleChar) )
      private def TripleChars       = rule( rep(Interpolation | TripleChar) )
      private def TripleInterp      = rule( TripleStart ~ TripleChars ~ TripleEnd )
      private def SingleInterp      = rule( Quote ~ SingleChars ~ Quote )
      private def Interpolation: R0 = rule(
          InterpId
        | "$$"
        | test(inPattern) ~ "${" ~ ( '{' ~ CasePattern ~ '}' | CasePattern ) ~ "}"
        | "${" ~ ImpliedBlock ~ WL ~ "}"
      )
      def Rule = rule(
          NumericLiteral
        | BooleanLiteral
        | CharacterLiteral
        | Identifiers.Id ~ ( TripleInterp | "\"\\\"" | SingleInterp )
        | TripleString
        | SingleString
        | SymbolLiteral
        | NullLiteral
      )
    }

    object InPattern extends PatternSensitive { def inPattern = true }
    object OutsidePattern extends PatternSensitive { def inPattern = false }

    def Pattern = rule( WL ~ InPattern.Rule )
    def Expr    = rule( WL ~ OutsidePattern.Rule )
  }
}
