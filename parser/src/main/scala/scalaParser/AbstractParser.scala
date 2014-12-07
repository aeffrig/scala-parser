package scalaParser

import org.parboiled2._

trait AbstractParser extends Parser {
  def input: ParserInput

  type Type
  type Term
  type Variable[A]
  type Def <: Term

  type Block
  type CompilationUnit
  type Expr <: Term
  type Ident <: Term
  type Keyword
  type Literal <: Term
  type Mods
  type Pattern <: Def
  type Stat
  type TopLevel

  type TParam <: Variable[Type]
  type VParam <: Variable[Term]

  def VParams: Rule1[Seq[VParam]]
  def TParams: Rule1[Seq[TParam]]
  def VParam: Rule1[VParam]
  def TParam: Rule1[TParam]
  def Block: Rule1[Block]
  def CompilationUnit: Rule1[CompilationUnit]
  def Def: Rule1[Def]
  def Expr: Rule1[Expr]
  def Ident: Rule1[Ident]
  def Keyword: Rule1[Keyword]
  def Literal: Rule1[Literal]
  def Mods: Rule1[Mods]
  def Pattern: Rule1[Pattern]
  def Stat: Rule1[Stat]
  def TopLevel: Rule1[TopLevel]
  def Type: Rule1[Type]

  def memberDef(mods: Mods, key: Keyword, id: Ident, tparams: Seq[TParam], vparams: Seq[VParam], members: Seq[Def]): Def
}
