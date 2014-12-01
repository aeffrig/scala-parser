package scalaParser
package syntax

import org.parboiled2._

trait Keywords {
  self: Parser with Basic with Identifiers with Literals =>

  /**
   * Most keywords don't just require the correct characters to match,
   * they have to ensure that subsequent characters *don't* match in
   * order for it to be a keyword. This enforces that rule for key-words
   * (W) and key-operators (O) which have different non-match criteria.
   */
  object K {
    def W(s: String) = rule{ WL ~ Key.W(s) }
    def O(s: String) = rule{ WL ~ Key.O(s) }
  }

  def `:`  = rule( K.O(":") )
  def `*`  = rule( K.O("*") )
  def `=>` = rule( K.O("=>") | K.O("⇒") )
  def `=`  = rule( K.O("=") )
  def `@`  = rule( K.O("@") )
  def `<-` = rule( K.O("<-") | K.O("←") )
  def `<%` = rule( K.O("<%") )
  def `<:` = rule( K.O("<:") )
  def `>:` = rule( K.O(">:") )
  def `#`  = rule( K.O("#") )

  def `_`         = rule( K.W("_") )
  def `abstract`  = rule( K.W("abstract") )
  def `case`      = rule( K.W("case") )
  def `catch`     = rule( K.W("catch") )
  def `class`     = rule( K.W("class") )
  def `def`       = rule( K.W("def") )
  def `do`        = rule( K.W("do") )
  def `else`      = rule( K.W("else") )
  def `extends`   = rule( K.W("extends") )
  def `false`     = rule( K.W("false") )
  def `final`     = rule( K.W("final") )
  def `finally`   = rule( K.W("finally") )
  def `forSome`   = rule( K.W("forSome") )
  def `for`       = rule( K.W("for") )
  def `if`        = rule( K.W("if") )
  def `implicit`  = rule( K.W("implicit") )
  def `import`    = rule( K.W("import") )
  def `lazy`      = rule( K.W("lazy") )
  def `macro`     = rule( K.W("macro") )
  def `match`     = rule( K.W("match") )
  def `new`       = rule( K.W("new") )
  def `null`      = rule( K.W("null") )
  def `object`    = rule( K.W("object") )
  def `override`  = rule( K.W("override") )
  def `package`   = rule( K.W("package") )
  def `private`   = rule( K.W("private") )
  def `protected` = rule( K.W("protected") )
  def `return`    = rule( K.W("return") )
  def `sealed`    = rule( K.W("sealed") )
  def `super`     = rule( K.W("super") )
  def `this`      = rule( K.W("this") )
  def `throw`     = rule( K.W("throw") )
  def `trait`     = rule( K.W("trait") )
  def `true`      = rule( K.W("true") )
  def `try`       = rule( K.W("try") )
  def `type`      = rule( K.W("type") )
  def `val`       = rule( K.W("val") )
  def `var`       = rule( K.W("var") )
  def `while`     = rule( K.W("while") )
  def `with`      = rule( K.W("with") )
  def `yield`     = rule( K.W("yield") )
}
