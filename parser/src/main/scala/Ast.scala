package psp
package parser
package ast

/** A strawman AST.
 */

/** Example:

  sealed class Foo[A <: Zing](x: Int = 5) extends Bar with Baz {
    def f = 1
    protected type Quux >: Null
  }

  TypeDef(
    Mods(SEALED, CLASS),
    TypeId(Foo),
    Constraints(
      HasParams(TypeDef(Mods(PARAM), TypeId(A), HasSuperType(TypeId(Zing)))),
      HasParams(TermDef(Mods(PARAM), TermId(x), HasSameType(TypeId(Int)), HasValue(Lit(5))),
      HasSuperType(TypeId(Bar)),
      HasSuperType(TypeId(Baz)),
      HasMember(TermDef(Mods(DEF), TermId(f), HasValue(Lit(1)))),
      HasMember(TypeDef(Mods(PROTECTED, TYPE), TypeId(Quux), HasSubType(TypeId(Null))))
    )
  )

 **/

sealed trait Ast  extends Any
sealed trait Term extends Any with Ast
sealed trait Type extends Any with Ast
sealed trait Mod  extends Any with Ast // including keywords e.g. trait, class, object, type, def
sealed trait Stat extends Any with Ast // anything which can stand on its own (no fragments like CaseDef, ImportExpr, etc)
sealed trait Ref  extends Any with Ast // a (possibly qualified) name referring to a type or term
sealed trait Def  extends Any with Ast // a template def, member def, type or value parameter - something which introduces one or more names

sealed trait Expr    extends Any with Stat with Term
sealed trait Pat     extends Any with Def
sealed trait TermRef extends Any with Ref with Expr // simple or qualified terms
sealed trait TypeRef extends Any with Ref with Type // simple or (term-)qualified types
sealed trait Id      extends Any with Ref           // unqualified identifiers

final case class SomeId(chars: String)                              extends AnyVal with Id               // indeterminate namespace simple id, e.g. the Baz in import foo.bar.Baz
final case class TermId(chars: String)                              extends AnyVal with Id with TermRef  // simple term
final case class TypeId(chars: String)                              extends AnyVal with Id with TypeRef  // simple type
final case class TermSelect(qual: TermRef, name: TermId)            extends TermRef                      // qualified term
final case class TypeSelect(qual: TermRef, name: TypeId)            extends TypeRef                      // qualified type
final case class TermDef(mods: Mods, name: TermId, cs: Constraints) extends Def                          // including methods, vals, vars, objects, packages
final case class TypeDef(mods: Mods, name: TypeId, cs: Constraints) extends Def                          // including classes, traits, abstract types, type aliases
final case class CaseDef(pat: Pat, body: Expr)                      extends Def                          // Ast fragment which uglifies the whole neighborhood.
final case class ValDefs(mods: Mods, pats: Pats, cs: Constraints)   extends Def                          // A real wart on uniformity of member definitions.

sealed trait Constraint extends Any with Ast
final case class HasValue(expr: Expr)    extends Constraint // method bodies, default arguments
final case class HasMember(defn: Def)    extends Constraint // including structural types and member implementations
final case class HasImplicit(tp: Type)   extends Constraint // context and view bounds
final case class HasParams(params: Defs) extends Constraint // all value and type parameters (with higher-order params as constraints on the parameter defs)
final case class HasSameType(tp: Type)   extends Constraint // parameter types, method return types, type alias bodies
final case class HasSubType(tp: Type)    extends Constraint // lower bounds (abstract types and type params)
final case class HasSuperType(tp: Type)  extends Constraint // upper bounds, but also class/trait/etc parents

sealed trait ImportExpr extends Any with Ast
final case class ImportId(name: Id)             extends ImportExpr
final case class ImportRename(from: Id, to: Id) extends ImportExpr
final case class ImportHide(name: Id)           extends ImportExpr
final case object ImportWildcard                extends ImportExpr

final case class Import(qual: TermRef, exprs: ImportExprs) extends Stat

object Pat {
  final case class Alternative(pats: Pats)               extends Pat
  final case class Bind(id: TermId, Pat: Pat)            extends Pat
  final case class Constructor(fn: Expr, args: Pats)     extends Pat
  final case class Guarded(pat: Pat, guard: Expr)        extends Pat
  final case class Infix(lhs: Pat, id: TermId, rhs: Pat) extends Pat
  final case class Lit[A](lit: Expr.Lit[A])              extends Pat
  final case class Product(pats: Pats)                   extends Pat
  final case class Typed(pat: Pat, tpe: Type)            extends Pat
}
object Type {
  final case class Applied(con: Type, args: Types)            extends Type
  final case class Single(value: TermRef)                     extends Type
  final case class Projection(qual: TypeRef, name: TypeId)    extends Type
  final case class FunctionType(in: Types, out: Type)         extends Type
  final case class Compound(types: Types)                     extends Type
  final case class Constrained(base: Type, cs: Constraints)   extends Type // including refinement types, existential types, polytypes
}
object Expr {
  final case class Apply(fn: Expr, args: Exprs)                extends Expr
  final case class Block(stats: Stats)                         extends Expr
  final case class Ident(name: TermId)                         extends Expr
  final case class Lit[A](value: A)                            extends Expr
  final case class Match(expr: Expr, cases: CaseDefs)          extends Expr
  final case class Select(qual: Expr, name: TermId)            extends Expr
  final case class Super(enclosing: TypeId, qualifier: TypeId) extends Expr
  final case class This(enclosing: TypeId)                     extends Expr
  final case class Typed(tpe: Type)                            extends Expr
}
object Mod {
  final case class Keyword(value: String) extends Mod

  final val Class   = Keyword("class")
  final val Trait   = Keyword("trait")
  final val Private = Keyword("private")
  // etc
}

sealed abstract class Asts[+A](xs: Vector[A]) extends Ast
final case class CaseDefs(xs: CaseDef*)       extends Asts[CaseDef](xs.toVector)
final case class Constraints(xs: Constraint*) extends Asts[Constraint](xs.toVector)
final case class Defs(xs: Def*)               extends Asts[Def](xs.toVector)
final case class Exprs(xs: Expr*)             extends Asts[Expr](xs.toVector)
final case class Mods(xs: Mod*)               extends Asts[Mod](xs.toVector)
final case class Pats(xs: Pat*)               extends Asts[Pat](xs.toVector)
final case class ImportExprs(xs: ImportExpr*) extends Asts[ImportExpr](xs.toVector)
final case class Stats(xs: Stat*)             extends Asts[Stat](xs.toVector)
final case class TermIds(xs: TermId*)         extends Asts[TermId](xs.toVector)
final case class Types(xs: Type*)             extends Asts[Type](xs.toVector)
