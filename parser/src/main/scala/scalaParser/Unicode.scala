package psp
package parser

import org.parboiled2._
import CharPredicate.{ HexDigit, AlphaNum, Digit19 }
import psp.parser.macros.Macros._
import java.lang.Integer.parseInt
import java.lang.{ Character => C }, C.UnicodeBlock

object CharacterCategory {
  final val CombiningSpacingMark    = java.lang.Character.COMBINING_SPACING_MARK
  final val ConnectorPunctuation    = java.lang.Character.CONNECTOR_PUNCTUATION
  final val Control                 = java.lang.Character.CONTROL
  final val CurrencySymbol          = java.lang.Character.CURRENCY_SYMBOL
  final val DashPunctuation         = java.lang.Character.DASH_PUNCTUATION
  final val DecimalDigitNumber      = java.lang.Character.DECIMAL_DIGIT_NUMBER
  final val EnclosingMark           = java.lang.Character.ENCLOSING_MARK
  final val EndPunctuation          = java.lang.Character.END_PUNCTUATION
  final val FinalQuotePunctuation   = java.lang.Character.FINAL_QUOTE_PUNCTUATION
  final val Format                  = java.lang.Character.FORMAT
  final val InitialQuotePunctuation = java.lang.Character.INITIAL_QUOTE_PUNCTUATION
  final val LetterNumber            = java.lang.Character.LETTER_NUMBER
  final val LineSeparator           = java.lang.Character.LINE_SEPARATOR
  final val LowercaseLetter         = java.lang.Character.LOWERCASE_LETTER
  final val MathSymbol              = java.lang.Character.MATH_SYMBOL
  final val ModifierLetter          = java.lang.Character.MODIFIER_LETTER
  final val ModifierSymbol          = java.lang.Character.MODIFIER_SYMBOL
  final val NonSpacingMark          = java.lang.Character.NON_SPACING_MARK
  final val OtherLetter             = java.lang.Character.OTHER_LETTER
  final val OtherNumber             = java.lang.Character.OTHER_NUMBER
  final val OtherPunctuation        = java.lang.Character.OTHER_PUNCTUATION
  final val OtherSymbol             = java.lang.Character.OTHER_SYMBOL
  final val ParagraphSeparator      = java.lang.Character.PARAGRAPH_SEPARATOR
  final val PrivateUse              = java.lang.Character.PRIVATE_USE
  final val SpaceSeparator          = java.lang.Character.SPACE_SEPARATOR
  final val StartPunctuation        = java.lang.Character.START_PUNCTUATION
  final val Surrogate               = java.lang.Character.SURROGATE
  final val TitlecaseLetter         = java.lang.Character.TITLECASE_LETTER
  final val Unassigned              = java.lang.Character.UNASSIGNED
  final val UppercaseLetter         = java.lang.Character.UPPERCASE_LETTER
}

final class CodePoint private (private val value: Int) extends AnyVal {
  import CharacterCategory._

  def isControl    = C isISOControl value
  def isLower      = C isLowerCase value
  def isUpper      = C isUpperCase value
  def isAlpha      = C isLetterOrDigit value
  def isDigit      = C isDigit value
  def isLetter     = C isLetter value
  def isIdentStart = C isUnicodeIdentifierStart value
  def isIdentPart  = C isUnicodeIdentifierPart value
  def isSpace      = C isWhitespace value
  def toChar       = value.toChar
  def toChars      = C toChars value
  def length       = C charCount value
  def category     = C getType value
  def block        = UnicodeBlock of value

  def isSevenBitAscii = value < 128
  def isUnicodeOperator = !isSevenBitAscii && {
    category match {
      case OtherSymbol | MathSymbol => true
      case _                        => false
    }
  }
  def isOperator = toChar match {
    case '\\' | '!' | '#' | '$' | '%' | '&' | '*' | '+' | '-' | ':' | '<' | '=' | '>' | '?' | '@' | '^' | '|' | '~' => true
    case '\u007f'                                                                                                   => true // XXX scala bug.
    case _                                                                                                          => isUnicodeOperator
  }
  def isPrintable = block match {
    case null | UnicodeBlock.SPECIALS => false
    case _                            => !isControl
  }

  override def toString = s"'$toChar'"
}
object CodePoint {
  def apply(ch: Char): CodePoint = (
    if (C isSurrogate ch)
      abort(s"Surrogate character: $ch")
    else
      apply(ch.toInt)
  )
  def apply(hex: String): CodePoint = apply(parseInt(hex, 16))
  def apply(value: Int): CodePoint = {
    if (C isValidCodePoint value)
      new CodePoint(value)
    else
      abort(s"Invalid code point: $value")
  }
}
