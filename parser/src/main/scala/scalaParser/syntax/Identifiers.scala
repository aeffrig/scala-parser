package scalaParser
package syntax

import org.parboiled2._
import psp.std._, api._

trait Identifiers {
  self: Parser with Basic =>

  object Identifiers {
    import Basic._

    def VarId   = EncodedVarId
    def PlainId = EncodedPlainId

    def Operator       = rule( !Keywords ~ oneOrMore(OperatorChar) )
    def EncodedVarId   = rule( !Keywords ~ Lower ~ EncodedIdRest )
    def RawVarId       = rule( !Keywords ~ Lower ~ RawIdRest )
    def EncodedPlainId = rule( !Keywords ~ Upper ~ EncodedIdRest | EncodedVarId | Operator )
    def RawPlainId     = rule( !Keywords ~ Upper ~ RawIdRest | RawVarId | Operator )
    def Id             = rule( !Keywords ~ EncodedPlainId | BacktickedId )
    def IdOrKeyword    = rule( PlainId | Keywords )
    def BacktickedId   = rule( "`" ~ oneOrMore(noneOf("`")) ~ "`" )

    private def EncodedIdRest  = rule( zeroOrMore(zeroOrMore("_") ~ oneOrMore(!"_" ~ AlphaNum)) ~ UnderscorePart )
    private def RawIdRest      = rule( zeroOrMore(zeroOrMore("_") ~ oneOrMore(!anyOf("_$") ~ AlphaNum)) ~ UnderscorePart )
    private def UnderscorePart = rule( optional(oneOrMore("_") ~ zeroOrMore(OperatorChar)) )

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
