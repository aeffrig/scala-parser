package psp
package parser

import org.parboiled2._
import psp.std._, api._
import macros.Macros._

trait Identifiers {
  self: PspParser =>

  object Identifiers {
    import Basic._

    def VarId            = EncodedVarId
    def PlainId          = EncodedPlainId
    def Backtick         = rule( '`' )
    def TripleTick       = rule( Backtick ~ Backtick ~ Backtick )
    def BacktickedId: R0 = rule( Backtick ~ rep1(!Backtick ~ ANY) ~ Backtick )
    def TickEmbedded     = rule( WS ~ TripleTick ~ Ident ~ WL ~ rep(!TripleTick ~ ANY) ~ opt(WL) ~ TripleTick )

    def Operator       = rule( !Keywords ~ rep1(OperatorChar) )
    def EncodedVarId   = rule( !Keywords ~ Lower ~ EncodedIdRest )
    def RawVarId       = rule( !Keywords ~ Lower ~ RawIdRest )
    def EncodedPlainId = rule( !Keywords ~ Upper ~ EncodedIdRest | EncodedVarId | Operator )
    def RawPlainId     = rule( !Keywords ~ Upper ~ RawIdRest | RawVarId | Operator )
    def Ident          = rule( !Keywords ~ EncodedPlainId | BacktickedId )
    def IdOrKeyword    = rule( PlainId | Keywords )

    private def EncodedIdRest  = rule( rep(rep("_") ~ rep1(!"_" ~ AlphaNum)) ~ UnderscorePart )
    private def RawIdRest      = rule( rep(rep("_") ~ rep1(!anyOf("_$") ~ AlphaNum)) ~ UnderscorePart )
    private def UnderscorePart = rule( optional(rep1("_") ~ rep(OperatorChar)) )

    def AlphabetKeywords = rule {
      (
        "abstract" | "case" | "catch" | "class" | "def" | "do" | "else" | "extends" | "false" | "finally" | "final" | "finally" | "forSome" | "for" | "if" |
        "implicit" | "import" | "lazy" | "match" | "new" | "null" | "object" | "override" | "package" | "private" | "protected" | "return" |
        "sealed" | "super" | "this" | "throw" | "trait" | "try" | "true" | "type" | "val" | "var" | "while" | "with" | "yield" | "_"
      ) ~ !AlphaNum
    }
    def SymbolicKeywords = rule{
      (
        ":" | ";" | "=>" | "=" | "<-" | "<:" | "<%" | ">:" | "#" | "@" | "\u21d2" | "\u2190"
      )  ~ !OperatorChar
    }
    def Keywords = rule( AlphabetKeywords | SymbolicKeywords )
  }
}
