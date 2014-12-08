package psp
package parser

import org.parboiled2._
import psp.parser.macros.Macros._
import psp.std.{ path, Path }
import psp.std.ansi._

// These Function0 wrappers are horrific but seem necessary in current parboiled.
class ScalaParser(val input: ParserInput) extends PspParser with Keywords with Xml {
  /** Note how nested constructs are differentiated from top level ones.
   *  At the top level, val/var/def/type are disallowed.
   *  Within a template, package definitions are disallowed.
   */
  val TopStat            : F0R0 = () => rule( Import | TemplateDef | PackageDef )
  val BlockStat          : F0R0 = () => rule( Import | MemberDef | BlockExpr )

  val AnnotatedTypeParam : F0R0 = () => rule( Annotations ~ TypeParam )
  val FlatPackageStat    : F0R0 = () => rule( Package ~ QualId ~ !BlockStart )
  val UnmodifiedDefs     : F0R0 = () => rule( inBraces(() => Unmodified.Def) )
  val VariantTypeParam   : F0R0 = () => rule( Annotations ~ opt(WL ~ Variance) ~ TypeParam )
  val Parents            : F0R0 = () => rule( opt(EarlyDefs) ~ IntersectionType )

  val Refinement     : F0R0 = () => rule( OneNLMax ~ UnmodifiedDefs() )
  val BlockStatSeq   : F0R0 = () => semiSeparated(BlockStat)
  val TopStatSeq     : F0R0 = () => semiSeparated(TopStat)
  val PackageStatSeq : F0R0 = () => semiSeparated(FlatPackageStat) // package foo.bar statements at the very top level
  val Template       : F0R0 = () => rule( '{' ~ opt(SelfType ~ RArrow) ~ BlockStatSeq() ~ '}' )

  val IntersectionTypeF0 = () => IntersectionType
  val TypeArgF0          = () => TypeArg
  val ParamTypeF0        = () => ParamType
  val PatternF0          = () => Pattern

  /** File-level entry point.
   */
  def CompilationUnit: Rule1[String] = rule( capture(CompUnit) ~ EOI )

  /** Big Picture rules.
   */

  def CompUnit    = rule( PackageStatSeq() ~ TopStatSeq() ~ WL )
  def MemberDef   = rule( AnnotationsAndMods ~ Unmodified.Def )
  def SubExpr     = rule( InExpr.Expr )
  def BlockExpr   = rule( InBlock.Expr )
  def Pattern     = rule( rep1sep(PatternAlternative, Pipe) )
  def TemplateDef = rule( AnnotationsAndMods ~ Unmodified.TemplateDef )
  def Type: R0    = rule( InfixType ~ opt(ExistentialPart | RArrow ~ Type) )

  /**
   * By default, all strings and characters greedily
   * capture all whitespace immediately before the token.
   */
  implicit def wspStr(s: String): R0 = rule( WL ~ str(s) )
  implicit def wspChar(s: Char): R0  = rule( WL ~ ch(s) )

  def At            = rule( `@` )
  def CaseOrPackage = rule( `case` | `package` )
  def Colon         = rule( `:` )
  def Comma         = rule( ',' )
  def Dot           = rule( '.' )
  def Equals        = rule( `=` )
  def Hash          = rule( `#` )
  def LArrow        = rule( `<-` )
  def Package       = rule( `package` )
  def Pipe          = rule( `|` )
  def RArrow        = rule( `=>` )
  def Star          = rule( `*` )
  def SubType       = rule( `<:` )
  def SuperType     = rule( `>:` )
  def Uscore        = rule( `_` )
  def ValOrVar      = rule( `val` | `var` )
  def Variance      = rule( Plus | Minus )
  def ViewBound     = rule( `<%` )
  def WildcardStar  = rule( Uscore ~ Star )

  /**
   * helper printing function
   */
  def pr(s: String) = rule( run(println("%4s  %s".format(cursor, s))) )

  /** Whitespace rules.
   */
  def CommentWS    = rule( SpaceWS ~ Literals.Comment ~ SpaceWS ~ Basic.Newline )
  def NL           = rule( WL ~ Basic.Newline )
  def NotNL: R0    = rule( &( WS ~ !Basic.Newline ) )
  def OneNLMax: R0 = rule( OptNL ~ rep(CommentWS) ~ NotNL )
  def OptNL        = rule( WS ~ opt(Basic.Newline) )
  def OptSemi      = rule( opt(Semi) )
  def OptSemis     = rule( opt(Semis) )
  def Semi         = rule( WS ~ Basic.Semi )
  def Semis        = rule( rep1(Semi) )
  def SpaceWS      = rule( rep(Basic.WhitespaceChar) )

  /** Identifier rules, including the identifier-like _, this, and super.
   */
  def Id            = rule( WL ~ Identifiers.Id )
  def IdDot         = rule( Id ~ Dot )
  def IdOrThis      = rule( Id | This )
  def IdOrUscore    = rule( Id | Uscore )
  def QualId        = rule( WL ~ rep1sep(Id, Dot) )
  def QualSuper     = rule( `super` ~ opt(Qualifier) )
  def Qualifier     = rule( '[' ~ IdOrThis ~ ']' )
  def StableId: R0  = rule( rep1sep(Id | This | QualSuper, Dot) )
  def This          = rule( `this` )
  def VarId         = rule( WL ~ Identifiers.VarId )
  def VarIdOrUscore = rule( VarId | Uscore )

  /** Type rules (involving the direct expression of types.)
   */
  def CompoundType     = oneOrBoth(IntersectionTypeF0, Refinement)                     // A { B } where A or { B } but not both may be absent
  def ExistentialPart  = rule( `forSome` ~ UnmodifiedDefs() )                          // A forSome { type B ; def C }
  def InfixType: R0    = rule( rep1sep(CompoundType, NotNL ~ Id ~ OneNLMax) )          // A \/ B
  def ParamType: R0    = rule( Type ~ opt(Star) | RArrow ~ Type )                      // A* or => B
  def ParentType       = rule( SimpleType ~ rep(NotNL ~ Annotation) ~ rep(SuperArgs) ) // A @B (C, D)
  def IntersectionType = rule( rep1sep(ParentType, `with`) )                           // Hey, we can pass arguments to multiple parents (i.e. trait constructors)
  def ProductType      = inParens(ParamTypeF0)                                         // (A, B)
  def SimpleType       = rule( AtomicType ~ rep(TypeSuffix) )                          // A#B, A.type#B
  def TypeArgs         = inBrackets(TypeArgF0)                                         // [A, B, ...]
  def ExprType         = rule( WildcardStar | Type | rep1(Annotation) )                // x: _* or (x: @switch) or x: A

  def SelfType = rule(
      `this` ~ AscriptedInfix.Type // this: A
    | Uscore ~ AscriptedInfix.Type // _: A
    | Id ~ AscriptedInfix.Opt      // x: A or x
  )
  def AtomicType = rule(
      Uscore        // wildcard type
    | ProductType   // including ()
    | StableId      // type selection
  )
  def TypeSuffix = rule(
      Dot ~ `type`  // singleton type
    | Hash ~ Id     // type projection
    | TypeArgs      // type application
  )

  /** Pattern rules.
   */
  def BindablePattern        = rule( opt(VarIdOrUscore ~ At) ~ ( WildcardStar | InfixPattern ) )
  def CaseClause: R0         = rule( `case` ~ Pattern ~ opt(InExpr.Guard) ~ RArrow ~ ImpliedBlock )
  def CaseBlock              = rule( '{' ~ rep1(CaseClause) ~ '}' )
  def ConstructorPattern     = rule( StableId ~ opt(TypeArgs) ~ opt(ProductPattern) )
  def InfixPattern           = rule( rep1sep(SimplePattern, Id) )
  def PatternAlternative: R0 = rule( TypedPattern | BindablePattern )
  def Patterns               = rule( rep1sep(BindablePattern ~ opt(RArrow ~ BindablePattern), Comma) )
  def ProductPattern         = rule( inParens(PatternF0) )
  def TypedPattern           = rule( VarIdOrUscore ~ AscriptedPattern.Type )
  def UnderscorePattern      = rule( Uscore ~ !Star ~ AscriptedPattern.Opt )
  def SimplePattern          = rule(
      XmlPattern
    | UnderscorePattern
    | Literals.Pattern
    | ProductPattern
    | ConstructorPattern
    | VarId // variable pattern
  )

  /** Whitespace-sensitive rules, mostly impacting expressions.
   *  The distinction is that sometimes newlines are significant and must be inferred
   *  as a semicolon, and sometimes they are simply whitespace.
   */
  object InBlock extends WhitespaceRules(inBlock = true)
  object InExpr extends WhitespaceRules(inBlock = false)

  abstract class WhitespaceRules(inBlock: Boolean) {
    def OptionalNewlineInBlock: R0 = if (inBlock) OneNLMax else MATCH
    def NoNewlineInBlock: R0       = if (inBlock) NotNL else MATCH

    // These rules can all commit after the keyword.
    def CatchPart  = rule( `catch`   ~!~ Expr )
    def DoPart     = rule( `do`      ~!~ Expr )
    def ElsePart   = rule( `else`    ~!~ Expr )
    def FinPart    = rule( `finally` ~!~ Expr )
    def ForExpr    = rule( `for`     ~!~ EnumeratorsPart ~ ( YieldPart | Expr ) )
    def Guard      = rule( `if`      ~!~ PostfixExpr )
    def IfExpr     = rule( `if`      ~!~ ParenExpr ~ ThenElsePart )
    def MatchPart  = rule( `match`   ~!~ CaseBlock )
    def NewExpr    = rule( `new`     ~!~ ExtendsOrNew )
    def ReturnExpr = rule( `return`  ~!~ opt(Expr) )
    def ThrowExpr  = rule( `throw`   ~!~ Expr )
    def TryExpr    = rule( `try`     ~!~ Expr ~ TryRest )
    def WhilePart  = rule( `while`   ~!~ ParenExpr )
    def YieldPart  = rule( `yield`   ~!~ Expr )

    def ThenElsePart = rule( Expr ~ opt(OptSemi ~ ElsePart) )
    def TryRest      = rule( CatchPart ~ opt(FinPart) | FinPart | MATCH ) // XXX MATCH is a scala bug - naked try
    def DoExpr       = rule( DoPart ~ OptSemi ~ WhilePart )
    def WhileExpr    = rule( WhilePart ~ Expr )
    def AssignPart   = rule( Equals ~ Expr )
    def InfixPart    = rule( NoNewlineInBlock ~ Id ~ opt(TypeArgs) ~ OptionalNewlineInBlock ~ PrefixExpr )
    def PostfixExpr  = rule( PrefixExpr ~ rep(InfixPart) ~ opt(PostfixPart) )
    def PostfixPart  = rule( NoNewlineInBlock ~ Id ~ opt(NL) ) // not OptNL!
    def PrefixExpr   = rule( opt(PrefixOperator) ~ SimpleExpr )

    /** For comprehensions.
     */
    def Enumerator      = rule( Generator | ForAssign | Guard )
    def Enumerators     = rule( Generator ~ rep(Semis ~ Enumerator) ~ WL )
    def ForAssign       = rule( opt(`val`) ~ PatternAlternative ~ AssignPart ~ opt(Guard) )  // Leading val is deprecated
    def Generator       = rule( PatternAlternative ~ LArrow ~ Expr ~ opt(Guard) )
    def EnumeratorsPart = rule(
        '(' ~ InExpr.Enumerators ~ ')'
      | '{' ~ InBlock.Enumerators ~ '}'
    )

    def SimpleExprStart = rule(
        XmlExpr
      | NewExpr
      | ExplicitBlock
      | ProductExpr
      | Literals.Expr
      | StableId
      | Uscore
    )
    def SimpleExprPart = rule(
        TypeArgs
      | Dot ~ Id
      | NoNewlineInBlock ~ ArgumentExprs
    )
    def SimpleExpr: R0 = rule( SimpleExprStart ~ rep(SimpleExprPart) ~ opt(NoNewlineInBlock ~ Uscore) )
    def LambdaExpr     = rule( rep1(LambdaParams ~ RArrow) ~ ( Expr | ImpliedBlock ) )

    def Expr: R0 = rule(
        LambdaExpr
      | IfExpr
      | WhileExpr
      | TryExpr
      | DoExpr
      | ForExpr
      | ThrowExpr
      | ReturnExpr
      | PostfixExpr ~ opt(ExprSuffix)
    )

    def ExprSuffix = rule(
        MatchPart
      | AscriptedExpr.Type
      | AssignPart
    )
  }

  /** Inside parentheses, the params can be `Type`s.
   *  A naked single parameter can only be an `InfixType`
   *  because `Type`s may be function types containing =>
   *  which would be ambiguous with the => separating the
   *  parameter from the body. Thus,
   *    (x: A => B) => // ok
   *     x: A =>       // ok
   *     x: A => B =>  // parse error
   */
  def LambdaParams = rule(
      inParens(AscriptedParam.BindingF0)
    | opt(`implicit`) ~ AscriptedInfix.Binding
  )

  def SuperArgs       = rule( NotNL ~ ArgumentExprs )
  def ProductExpr     = rule( '(' ~ repsep(WL ~ SubExpr, Comma) ~ ')' )
  def ArgumentExprs   = rule( ProductExpr | ExplicitBlock )
  def ValueConstraint = rule(
      AscriptedParam.Type
    | Equals ~ opt(`macro`) ~ BlockExpr
  )

  def TypeConstraint = rule(
      SubType ~ Type          // <: Type   (upper bound)
    | SuperType ~ Type        // >: Type   (lower bound)
    | ViewBound ~ Type        // <% Type   (view bound)
    | Colon ~ Type            //  : Type   (context bound)
    | Equals ~ Type           //  = Type   (type alias definition, or potentially a type parameter default or named type argument)
    | TypeTypeParamList       // [Type...] (type parameter list)
  )
  def TypeArg   = rule( Type ~ rep(TypeConstraint) )
  def TypeParam = rule( IdOrUscore ~ rep(TypeConstraint) )

  sealed abstract class AscriptedType(tpe: => R0) {
    private val Rule: F0R0 = () => tpe

    val BindingF0: F0R0 = () => Binding

    def Self    = rule( Id ~ Opt | ( `this` | Uscore ) ~ Type )
    def Binding = rule( IdOrUscore ~ Opt)
    def Type    = rule( Colon ~ Rule() )
    def Opt     = rule( opt(Type) )
  }
  object AscriptedParam   extends AscriptedType(ParamType)
  object AscriptedPattern extends AscriptedType(CompoundType)
  object AscriptedInfix   extends AscriptedType(InfixType)
  object AscriptedExpr    extends AscriptedType(ExprType)

  def AccessModifier      = rule( ( `private` | `protected` ) ~ opt(Qualifier) )
  def AnnotArgument: R0   = rule( opt(Id ~ Equals) ~ ( !(Id ~ Colon) ~ SubExpr ) )
  def Annotation          = rule( At ~ SimpleType ~ opt(NotNL ~ AnnotationArguments) ) // needs SimpleType to accept type arguments
  def AnnotationArguments = rule( '(' ~ repsep(AnnotArgument, Comma) ~ ')' )
  def Annotations         = rule( rep(Annotation) )
  def AnnotationsAndMods  = rule( rep(Annotation ~ OneNLMax) ~ rep(Modifier) )
  def Block: R0           = ImpliedBlock
  def ClassObjectOrTrait  = rule( `class` | `object` | `trait` )
  def ConstructorMods     = rule( rep(Annotation ~ NotNL) ~ rep(Modifier) )
  def EarlyDefs           = rule( inBraces(() => MemberDef) ~ `with` )
  def ExplicitBlock: R0   = rule( OneNLMax ~ ( CaseBlock | '{' ~ BlockStatSeq() ~ '}' ) )
  def ExtendsOrNew        = oneOrBoth(Parents, Template)
  def ImpliedBlock: R0    = rule( OptSemis ~ BlockStatSeq() ~ BlockEnd )
  def Import              = rule( `import` ~ ImportExprs )
  def ImportExpr          = rule( StableId ~ opt(ImportSuffix) )
  def ImportExprs         = rule( rep1sep(ImportExpr, Comma) )
  def ImportSelector      = rule( IdOrUscore ~ opt(RArrow ~ IdOrUscore) )
  def ImportSelectors     = rule( '{' ~ rep1sep(ImportSelector, Comma) ~ '}' )
  def ImportSuffix        = rule( Dot ~ (Uscore | ImportSelectors) )
  def MethodParam         = rule( AnnotationsAndMods ~ opt(ValOrVar) ~ Id ~ rep(ValueConstraint) )
  def MethodParamList     = rule( '(' ~ opt(`implicit`) ~ repsep(MethodParam, Comma) ~ ')' )
  def MethodParamLists    = rule( rep(MethodParamList) )
  def MethodTypeParamList = inBrackets(AnnotatedTypeParam)
  def Modifier            = rule( `abstract` | `final` | `sealed` | `implicit` | `lazy` | `override` | AccessModifier )
  def PackageDef: R0      = rule( Package ~ QualId ~ inBraces(TopStat) )
  def ParenExpr           = rule( '(' ~ SubExpr ~ ')' )
  def TypeTypeParamList   = inBrackets(VariantTypeParam)
  def ValueConstraints    = rule( rep(ValueConstraint) )

  def errorContextWidth: Int            = 1
  def errorCharMarkup(ch: Char): String = {
    import scala.Console._
    RED + BOLD + REVERSED + ch + RESET
  }

  override def formatErrorProblem(error: ParseError): String = "Error"
  override def formatErrorLine(error: ParseError): String = {
    import error._, position._
    def line_s(i: Int): Option[String] = scala.util.Try(
      "%4d  %s".format(i,
        if (i != line) input getLine i
        else input getLine i splitAt column match { case (front, back) =>
          "" + (front dropRight 1) + errorCharMarkup(front.last) + back
        }
      )
    ).toOption
    (line - errorContextWidth) to (line + errorContextWidth) filter (_ >= 1) flatMap line_s mkString "\n"
  }

  /** Recombinators. */
  def oneOrBoth(p: F0R0, q: F0R0): R0 = rule( p() ~ opt(q()) | q() )
  def inBraces(stat: F0R0): R0        = rule( '{' ~ semiSeparated(stat) ~ '}' )
  def inParens(elem: F0R0): R0        = rule( '(' ~ repsep(elem(), Comma) ~ ')' )
  def inBrackets(param: F0R0): R0     = rule( '[' ~ rep1sep(param(), Comma) ~ ']' )
  def semiSeparated(stat: F0R0): R0   = rule( OptSemis ~ repsep(stat(), Semis) ~ OptSemis )

  object Unmodified {
    // The !RArrow in TemplateParams is defense against "class A" and "() => 5" on consecutive lines.
    private def DefIntro          = rule( `def` ~ IdOrThis ~ rep(MethodTypeParamList) ~ MethodParamLists )
    private def ConstructorParams = rule( opt(NotNL ~ ConstructorMods) ~ opt(MethodParamLists ~ !RArrow) )
    private def TemplateIntro     = rule( TemplateKeyword ~ Id ~ rep(TypeTypeParamList) ~ ConstructorParams )
    private def DefBody           = rule( ExplicitBlock | ValueConstraints )
    private def ExtendsClause     = rule( ( `extends` | SubType ) ~ ExtendsOrNew )
    private def TemplateOpt       = rule( ExtendsClause | opt(Template()) )

    def DefDef      = rule( DefIntro ~ DefBody )
    def TemplateDef = rule( TemplateIntro ~ TemplateOpt )
    def TypeDef     = rule( `type` ~ Id ~ rep(TypeConstraint) )
    def ValDef      = rule( ValOrVar ~ Patterns ~ ValueConstraints )

    /** Want a case trait? Sure. Need a package class? Sure, why not.
     */
    def TemplateKeyword = rule( opt(CaseOrPackage) ~ ClassObjectOrTrait )
    def Def: R0 = rule(
        ValDef
      | TypeDef
      | DefDef
      | TemplateDef
    )
  }

  private def BlockStart = rule( &( WS ~ '{' ) )
  private def BlockEnd   = rule( OptSemis ~ &( '}' | `case` ) )
}
