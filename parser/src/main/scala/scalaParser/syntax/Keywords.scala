package scalaParser
package syntax

import org.parboiled2._

trait Keywords {
  self: Parser with Basic with Identifiers with Literals =>

  /**
   *  Most keywords don't just require the correct characters to match,
   *  they have to ensure that subsequent characters *don't* match in
   *  order for it to be a keyword. Operators have different delimiting
   *  rules than do letters.
   */
  private def keyword(s: String)  = rule( WL ~ str(s) ~ !Basic.AlphaNum )
  private def operator(s: String) = rule( WL ~ str(s) ~ !Basic.OperatorChar )

  def `:`  = rule( operator(":") )
  def `*`  = rule( operator("*") )
  def `=>` = rule( operator("=>") | operator("⇒") )
  def `=`  = rule( operator("=") )
  def `@`  = rule( operator("@") )
  def `<-` = rule( operator("<-") | operator("←") )
  def `<%` = rule( operator("<%") )
  def `<:` = rule( operator("<:") )
  def `>:` = rule( operator(">:") )
  def `#`  = rule( operator("#") )
  def `|`  = rule( operator("|") )

  def `_`         = rule( keyword("_") )
  def `abstract`  = rule( keyword("abstract") )
  def `case`      = rule( keyword("case") )
  def `catch`     = rule( keyword("catch") )
  def `class`     = rule( keyword("class") )
  def `def`       = rule( keyword("def") )
  def `do`        = rule( keyword("do") )
  def `else`      = rule( keyword("else") )
  def `extends`   = rule( keyword("extends") )
  def `false`     = rule( keyword("false") )
  def `final`     = rule( keyword("final") )
  def `finally`   = rule( keyword("finally") )
  def `forSome`   = rule( keyword("forSome") )
  def `for`       = rule( keyword("for") )
  def `if`        = rule( keyword("if") )
  def `implicit`  = rule( keyword("implicit") )
  def `import`    = rule( keyword("import") )
  def `lazy`      = rule( keyword("lazy") )
  def `macro`     = rule( keyword("macro") )
  def `match`     = rule( keyword("match") )
  def `new`       = rule( keyword("new") )
  def `null`      = rule( keyword("null") )
  def `object`    = rule( keyword("object") )
  def `override`  = rule( keyword("override") )
  def `package`   = rule( keyword("package") )
  def `private`   = rule( keyword("private") )
  def `protected` = rule( keyword("protected") )
  def `return`    = rule( keyword("return") )
  def `sealed`    = rule( keyword("sealed") )
  def `super`     = rule( keyword("super") )
  def `this`      = rule( keyword("this") )
  def `throw`     = rule( keyword("throw") )
  def `trait`     = rule( keyword("trait") )
  def `true`      = rule( keyword("true") )
  def `try`       = rule( keyword("try") )
  def `type`      = rule( keyword("type") )
  def `val`       = rule( keyword("val") )
  def `var`       = rule( keyword("var") )
  def `while`     = rule( keyword("while") )
  def `with`      = rule( keyword("with") )
  def `yield`     = rule( keyword("yield") )
}
