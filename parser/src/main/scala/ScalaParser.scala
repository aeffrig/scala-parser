package psp
package parser

import org.parboiled2._
import psp.parser.macros.Macros._
import psp.std.{ path, Path }
import psp.std.ansi._

// These Function0 wrappers are horrific but seem necessary in current parboiled.
class ScalaParser(val input: ParserInput) extends PspParser with Keywords with Xml {
  def inputString = input.sliceString(0, input.length)
  override def toString = s"ScalaParser($inputString)"

  /** Note how nested constructs are differentiated from top level ones.
   *  At the top level, val/var/def/type are disallowed.
   *  Within a template, package definitions are disallowed.
   */
  val TopStat            : F0R0 = () => rule( Import | TemplateDef | PackageDef )
  val BlockStat          : F0R0 = () => rule( Import | MemberDef | BlockExpr )

  val MethodTypeParam    : F0R0 = () => rule( rep(Annotation) ~ TypeParam )                      // no variance annotation
  val VariantTypeParam   : F0R0 = () => rule( rep(Annotation) ~ opt(WL ~ Variance) ~ TypeParam ) // optional variance
  val UnmodifiedDefs     : F0R0 = () => rule( inBraces(() => Unmodified.Def) )
  val MemberDefs         : F0R0 = () => rule( inBraces(() => MemberDef) )

  val Parents        : F0R0 = () => rule( opt(MemberDefs() ~ `with`) ~ IntersectionType )    // optional early defs
  val Refinement     : F0R0 = () => rule( OneNLMax ~ UnmodifiedDefs() )
  val BlockStatSeq   : F0R0 = () => semiSeparated(BlockStat)
  val TopStatSeq     : F0R0 = () => semiSeparated(TopStat)
  val PackageStatSeq : F0R0 = () => semiSeparated(() => rule(PackageIntro ~ !BlockStart)) // package foo.bar statements at the very top level, not followed by a block
  val Template       : F0R0 = () => rule( '{' ~ opt(SelfType ~ RArrow) ~ BlockStatSeq() ~ '}' )

  val IntersectionTypeF0 = () => IntersectionType
  val TypeArgF0          = () => TypeArg
  val ParamTypeF0        = () => ParamType
  val PatternF0          = () => CasePattern

  // def csv0(in: ParserInput): Rule0 = rule( new csv.CsvParser(in, ',').file ~> ((x: Any) => println(x)) )

  /** File-level entry point.
   */
  def startRule = CompilationUnit

  /** Big Picture rules.
   */

  def BlockExpr       = rule( InBlock.Expr )
  def CasePattern: R0 = rule( rep1sep(ValPattern, Pipe) )
  def CompilationUnit = rule( PackageStatSeq() ~ TopStatSeq() ~ WL )
  def MemberDef       = rule( AnnotationsAndMods ~ Unmodified.Def )
  def SubExpr         = rule( WL ~ InExpr.Expr ) // i.e. expression outside block context
  def TemplateDef     = rule( AnnotationsAndMods ~ Unmodified.TemplateDef )
  def Type: R0        = rule( InfixType ~ opt(ExistentialPart | RArrow ~ Type) )

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
  def SequenceStar  = rule( Uscore ~ Star )
  def Star          = rule( `*` )
  def SubType       = rule( `<:` )
  def SuperType     = rule( `>:` )
  def Uscore        = rule( `_` )
  def ValOrVar      = rule( `val` | `var` )
  def Variance      = rule( Plus | Minus )
  def ViewBound     = rule( `<%` )

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
  def Id            = rule( WL ~ Identifiers.Ident )
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
  def ExprType         = rule( SequenceStar | Type | rep1(Annotation) )                // x: _* or (x: @switch) or x: A
  def InfixType: R0    = rule( rep1sep(CompoundType, NotNL ~ Id ~ OneNLMax) )          // A \/ B
  def IntersectionType = rule( rep1sep(ParentType, `with`) )                           // Hey, we can pass arguments to multiple parents (i.e. trait constructors)
  def ParamType: R0    = rule( Type ~ opt(Star) | RArrow ~ Type )                      // A* or => B
  def ParentType       = rule( SimpleType ~ rep(NotNL ~ Annotation) ~ rep(SuperArgs) ) // A @B (C, D)
  def ProductType      = inParens(ParamTypeF0)                                         // (A, B)
  def SimpleType       = rule( AtomicType ~ rep(TypeSuffix) )                          // A#B, A.type#B
  def TypeArgs         = inBrackets(TypeArgF0)                                         // [A, B, ...]

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

  def Bind               = rule( VarIdOrUscore ~ At ) // _ @ A seems pointless, but exists in the wild
  def BoundPattern       = rule( opt(Bind) ~ ( SequenceStar | InfixPattern ) )
  def CaseBlock          = rule( '{' ~ rep1(CaseClause) ~ '}' )
  def CaseBody           = rule( RArrow ~ ImpliedBlock )
  def CaseClause: R0     = rule( `case` ~ CasePattern ~ opt(InExpr.Guard) ~ CaseBody )
  def ConstructorPattern = rule( StableId ~ opt(TypeArgs) ~ opt(ProductPattern) )
  def FunPattern         = rule( BoundPattern ~ opt(RArrow ~ BoundPattern) ) // val _: (A, B) => C = x, seen in wild
  def FunPatterns        = rule( rep1sep(FunPattern, Comma) )
  def InfixPattern       = rule( rep1sep(SimplePattern, Id) )
  def ProductPattern     = rule( inParens(PatternF0) )
  def TypedPattern       = rule( VarIdOrUscore ~ AscriptedPattern.Type )
  def UnderscorePattern  = rule( Uscore ~ !Star ~ AscriptedPattern.Opt )
  def ValPattern         = rule( TypedPattern | BoundPattern )

  def SimplePattern      = rule(
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

  def discard(x: Any): Unit = println(x)
  // def EmbedExpr: R0         = rule( TripleTick ~ id ~ WS ~ (runSubParser(CsvParser(_).file) ~> discard) ~ TripleTick )

  abstract class WhitespaceRules(inBlock: Boolean) {
    import Identifiers._

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

    def AssignPart   = rule( Equals ~ Expr )
    def DoExpr       = rule( DoPart ~ OptSemi ~ WhilePart )
    def InfixPart    = rule( NoNewlineInBlock ~ Id ~ opt(TypeArgs) ~ OptionalNewlineInBlock ~ PrefixExpr )
    def PostfixExpr  = rule( PrefixExpr ~ rep(InfixPart) ~ opt(PostfixPart) )
    def PostfixPart  = rule( NoNewlineInBlock ~ Id ~ opt(NL) ) // not OptNL!
    def PrefixExpr   = rule( opt(PrefixOperator) ~ SimpleExpr )
    def ThenElsePart = rule( Expr ~ opt(OptSemi ~ ElsePart) )
    def TryRest      = rule( CatchPart ~ opt(FinPart) | FinPart | MATCH ) // XXX MATCH is a scala bug - naked try
    def WhileExpr    = rule( WhilePart ~ Expr )

    /** For comprehensions.
     */
    def Enumerator      = rule( Generator | ForAssign | Guard )
    def Enumerators     = rule( Generator ~ rep(Semis ~ Enumerator) ~ WL )
    def ForAssign       = rule( opt(`val`) ~ ValPattern ~ AssignPart ~ opt(Guard) )  // Leading val is deprecated
    def Generator       = rule( ValPattern ~ LArrow ~ Expr ~ opt(Guard) )
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
    // def InputLine = rule {
    //   oneOrMore(runSubParser(new SubParser(_).intNumber)).separatedBy(',') ~ EOI ~> (_.sum)
    // }
    // def csvSubParser(in: ParserInput) = rule( runSubParser(csvF1) )

    def Expr: R0 = rule(
        TickEmbedded
      | LambdaExpr
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
      inParens(AscriptedParam.Binding)
    | opt(`implicit`) ~ AscriptedInfix.Binding()
  )

  def SuperArgs       = rule( NotNL ~ ArgumentExprs )
  def ProductExpr     = rule( '(' ~ repsep(SubExpr, Comma) ~ ')' )
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
    | VariantTypeParamList    // [Type...] (type parameter list)
  )
  def TypeArg   = rule( Type ~ rep(TypeConstraint) )
  def TypeParam = rule( IdOrUscore ~ rep(TypeConstraint) )

  sealed abstract class AscriptedType(tpe: => R0) {
    private val Rule: F0R0 = () => tpe
    val Binding: F0R0 = () => rule( IdOrUscore ~ Opt )
    def Type = rule( Colon ~ Rule() )
    def Opt  = rule( opt(Type) )
  }
  object AscriptedParam   extends AscriptedType(ParamType)
  object AscriptedPattern extends AscriptedType(CompoundType)
  object AscriptedInfix   extends AscriptedType(InfixType)
  object AscriptedExpr    extends AscriptedType(ExprType)

  def AccessModifier       = rule( ( `private` | `protected` ) ~ opt(Qualifier) )
  def Annotation           = rule( At ~ SimpleType ~ opt(NotNL ~ ValueArguments) )
  def AnnotationsAndMods   = rule( rep(Annotation ~ OneNLMax) ~ rep(Modifier) )
  def ClassObjectOrTrait   = rule( `class` | `object` | `trait` )
  def ExplicitBlock: R0    = rule( OneNLMax ~ ( CaseBlock | '{' ~ BlockStatSeq() ~ '}' ) )
  def ExtendsOrNew         = oneOrBoth(Parents, Template)
  def ImpliedBlock: R0     = rule( OptSemis ~ BlockStatSeq() ~ BlockEnd )
  def Import               = rule( `import` ~ ImportExprs )
  def ImportExpr           = rule( StableId ~ opt(ImportSuffix) )
  def ImportExprs          = rule( rep1sep(ImportExpr, Comma) )
  def ImportSelector       = rule( IdOrUscore ~ opt(RArrow ~ IdOrUscore) )
  def ImportSelectors      = rule( '{' ~ rep1sep(ImportSelector, Comma) ~ '}' )
  def ImportSuffix         = rule( Dot ~ (Uscore | ImportSelectors) )
  def MethodParam          = rule( AnnotationsAndMods ~ opt(ValOrVar) ~ Id ~ rep(ValueConstraint) )
  def MethodParamList      = rule( '(' ~ opt(`implicit`) ~ repsep(MethodParam, Comma) ~ ')' )
  def MethodParamLists     = rule( rep(MethodParamList) )
  def MethodTypeParamList  = inBrackets(MethodTypeParam)
  def Modifier             = rule( `abstract` | `final` | `sealed` | `implicit` | `lazy` | `override` | AccessModifier )
  def PackageDef: R0       = rule( PackageIntro ~ inBraces(TopStat) )
  def PackageIntro         = rule( Package ~ QualId )
  def ParenExpr            = rule( '(' ~ SubExpr ~ ')' )
  def ValueArgument        = rule( opt(Id ~ Equals) ~ SubExpr )
  def ValueArguments       = rule( '(' ~ repsep(ValueArgument, Comma) ~ ')' )
  def VariantTypeParamList = inBrackets(VariantTypeParam)

  /** Recombinators. */
  def inBraces(stat: F0R0): R0        = rule( '{' ~ semiSeparated(stat) ~ '}' )
  def inBrackets(param: F0R0): R0     = rule( '[' ~ rep1sep(param(), Comma) ~ ']' )
  def inParens(elem: F0R0): R0        = rule( '(' ~ repsep(elem(), Comma) ~ ')' )
  def oneOrBoth(p: F0R0, q: F0R0): R0 = rule( p() ~ opt(q()) | q() )
  def semiSeparated(stat: F0R0): R0   = rule( OptSemis ~ repsep(stat(), Semis) ~ OptSemis )

  object Unmodified {
    // The !RArrow in TemplateParams is defense against "class A" and "() => 5" on consecutive lines.
    private def ConstructorMods   = rule( rep(Annotation ~ NotNL) ~ rep(Modifier) )
    private def ConstructorParams = rule( opt(NotNL ~ ConstructorMods) ~ opt(MethodParamLists ~ !RArrow) )
    private def DefBody           = rule( ExplicitBlock | rep(ValueConstraint) )  // i.e. could be { body }, : T, : T = body, but not " : T { body } "
    private def DefIntro          = rule( `def` ~ IdOrThis ~ rep(MethodTypeParamList) ~ MethodParamLists )
    private def ExtendsClause     = rule( ( `extends` | SubType ) ~ ExtendsOrNew )
    private def TemplateIntro     = rule( TemplateKeyword ~ Id ~ rep(VariantTypeParamList) ~ ConstructorParams )
    private def TemplateBody      = rule( ExtendsClause | Template() | MATCH )

    def DefDef      = rule( DefIntro ~ DefBody )
    def TemplateDef = rule( TemplateIntro ~ TemplateBody )
    def TypeDef     = rule( `type` ~ Id ~ rep(TypeConstraint) )
    def ValDef      = rule( ValOrVar ~ FunPatterns ~ rep(ValueConstraint) )

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
