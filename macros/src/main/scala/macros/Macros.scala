package scalaParser
package macros

import scala.language.experimental.macros
import org.parboiled2.{Rule0, RuleDSLCombinators, Rule}
import org.parboiled2.support._
import shapeless.HList
import scala.collection.immutable
import scala.reflect.macros.whitebox

object Macros {
  def opt[I <: HList, O <: HList]
         (r: Rule[I, O])
         (implicit o: Lifter[Option, I, O])
         : Rule[o.In, o.OptionalOut] = macro optional[I, O]

  def optional[I <: HList: c.WeakTypeTag, O <: HList: c.WeakTypeTag]
              (c: whitebox.Context)
              (r: c.Expr[Rule[I, O]])
              (o: c.Expr[Lifter[Option, I, O]])
              : c.Expr[Rule[o.value.In, o.value.OptionalOut]] = {
    import c.universe._
    c.Expr(q"optional($r)")
  }
  def rep[I <: HList, O <: HList]
         (r: Rule[I, O])
         (implicit s: Lifter[immutable.Seq, I, O])
         : Rule[s.In, s.OptionalOut] = macro rep0[I, O]

  def rep0[I <: HList: c.WeakTypeTag, O <: HList: c.WeakTypeTag]
          (c: whitebox.Context)
          (r: c.Expr[Rule[I, O]])
          (s: c.Expr[Lifter[immutable.Seq, I, O]])
          : c.Expr[Rule[s.value.In, s.value.OptionalOut]] = {
    import c.universe._
    c.Expr(q"zeroOrMore($r)")
  }
  def repsep[I <: HList, O <: HList]
            (r: Rule[I, O], sep: Rule0)
            (implicit s: Lifter[immutable.Seq, I, O])
            : Rule[s.In, s.OptionalOut] = macro repsep0[I, O]

  def repsep0[I <: HList: c.WeakTypeTag, O <: HList: c.WeakTypeTag]
              (c: whitebox.Context)
              (r: c.Expr[Rule[I, O]], sep: c.Expr[Rule0])
              (s: c.Expr[Lifter[immutable.Seq, I, O]])
              : c.Expr[Rule[s.value.In, s.value.OptionalOut]] = {
    import c.universe._
    c.Expr(q"zeroOrMore($r).separatedBy($sep)")
  }
  def rep1[I <: HList, O <: HList]
          (r: Rule[I, O])
          (implicit s: Lifter[immutable.Seq, I, O])
          : Rule[s.In, s.StrictOut] = macro rep10[I, O]

  def rep10[I <: HList: c.WeakTypeTag, O <: HList: c.WeakTypeTag]
           (c: whitebox.Context)
           (r: c.Expr[Rule[I, O]])
           (s: c.Expr[Lifter[immutable.Seq, I, O]])
           : c.Expr[Rule[s.value.In, s.value.StrictOut]] = {
    import c.universe._
    c.Expr(q"oneOrMore($r)")
  }
  def rep1sep[I <: HList, O <: HList]
            (r: Rule[I, O], sep: Rule0)
            (implicit s: Lifter[immutable.Seq, I, O])
            : Rule[s.In, s.StrictOut] = macro rep1sep0[I, O]

  def rep1sep0[I <: HList: c.WeakTypeTag, O <: HList: c.WeakTypeTag]
              (c: whitebox.Context)
              (r: c.Expr[Rule[I, O]], sep: c.Expr[Rule0])
              (s: c.Expr[Lifter[immutable.Seq, I, O]])
              : c.Expr[Rule[s.value.In, s.value.StrictOut]] = {
    import c.universe._
    c.Expr(q"oneOrMore($r).separatedBy($sep)")
  }
}
