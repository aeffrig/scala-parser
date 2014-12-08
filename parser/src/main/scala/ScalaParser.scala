package psp
package parser

import org.parboiled2._
import psp.parser.macros.Macros._
import psp.std.{ path, Path }
import psp.std.ansi._

abstract class PspParser extends Parser with Basic with Identifiers with Literals {
  def input: ParserInput

  def prevN(n: Int): String = input.sliceString(cursor - n, cursor)
  def nextN(n: Int): String = input.sliceString(cursor, cursor + n)

  def failMessage(path: Path, error: ParseError): String = {
    import error._, position._
    def pos_s = "%s:%s:%s".format(path, line, column)
    s"""|Expected: $formatExpectedAsString
        |  at $pos_s
        |
        |${this formatErrorLine error}
        |""".stripMargin.trim
  }

  /**
   * Parses all whitespace, excluding newlines. This is only
   * really useful in e.g. {} blocks, where we want to avoid
   * capturing newlines so semicolon-inference would work
   */
  def WS = rule( atomic(rep(Basic.WhitespaceChar | Literals.Comment)) )

  /**
   * Parses whitespace, including newlines.
   * This is the default for most things
   */
  def WL = rule( atomic(rep(Basic.WhitespaceChar | Literals.Comment | Basic.Newline)) )
}

class TraceScalaParser(in: ParserInput) extends ScalaParser(in) {
  val counts = scala.collection.mutable.Map[Int, Long]() withDefaultValue 0L
  private val reportFrequency = 100
  private var counted = 0L
  private def tick(): Unit = {
    counted += 1
    counts(cursor) += 1
    if (counted % reportFrequency == 0)
      println(counted / reportFrequency)
  }

  scala.sys addShutdownHook {
    println(s"\nCounted $counted rules.\n")
    counts.toList.sortBy(-_._2) foreach { case (k, v) => println("%6s  %s".format(k, v)) }
  }

  override def Expr = rule( super.Expr ~ run(tick()) )
  override def Type = rule( super.Type ~ run(tick()) )
}

// These Function0 wrappers are horrific but seem necessary in current parboiled.
class ScalaParser(val input: ParserInput) extends PspParser with Keywords with Xml {
  /** Note how nested constructs are differentiated from top level ones.
   *  At the top level, val/var/def/type are disallowed.
   *  Within a template, package definitions are disallowed.
   */
  val TopStat            : F0R0 = () => rule( Import | TemplateDef | PackageDef )
  val BlockStat          : F0R0 = () => rule( Import | MemberDef | InBlock.Expr )

  val AnnotatedTypeParam : F0R0 = () => rule( Annotations ~ TypeParam )
  val FlatPackageStat    : F0R0 = () => rule( Package ~ QualId ~ !BlockStart )
  val IntersectionType   : F0R0 = () => rule( rep1sep(ParentType, `with`) )
  val NameAndOptType     : F0R0 = () => rule( IdOrUscore ~ OptType )
  val UnmodifiedDefs     : F0R0 = () => rule( inBraces(() => Unmodified.Def) )
  val VariantTypeParam   : F0R0 = () => rule( Annotations ~ opt(WL ~ Variance) ~ TypeParam )
  val Parents            : F0R0 = () => rule( opt(EarlyDefs) ~ IntersectionType() )

  val Refinement     : F0R0 = () => rule( OneNLMax ~ UnmodifiedDefs() )
  val BlockStatSeq   : F0R0 = () => semiSeparated(BlockStat)
  val TopStatSeq     : F0R0 = () => semiSeparated(TopStat)
  val PackageStatSeq : F0R0 = () => semiSeparated(FlatPackageStat) // package foo.bar statements at the very top level
  val Template       : F0R0 = () => rule( '{' ~ opt(SelfType) ~ BlockStatSeq() ~ '}' )

  val ParamTypeF0 = () => ParamType
  val PatternF0   = () => Pattern

  /** File-level entry point.
   */
  def CompilationUnit: Rule1[String] = rule( capture(CompUnit) ~ EOI )

  /** Big Picture rules.
   */

  def CompUnit    = rule( PackageStatSeq() ~ TopStatSeq() ~ WL )
  def Expr        = rule( InExpr.Expr )
  def MemberDef   = rule( AnnotationsAndMods ~ Unmodified.Def )
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
  def Id               = rule( WL ~ Identifiers.Id )
  def IdDot            = rule( Id ~ Dot )
  def IdOrThis         = rule( Id | This )
  def IdOrUscore       = rule( Id | Uscore )
  def IdOrUscoreOrThis = rule( Id | Uscore | This )
  def QualId           = rule( WL ~ rep1sep(Id, Dot) )
  def QualSuper        = rule( `super` ~ opt(Qualifier) )
  def Qualifier        = rule( '[' ~ IdOrThis ~ ']' )
  def StableId: R0     = rule( rep1sep(Id | This | QualSuper, Dot) )
  def This             = rule( `this` )
  def VarId            = rule( WL ~ Identifiers.VarId )
  def VarIdOrUscore    = rule( VarId | Uscore )

  /** Type rules (involving the direct expression of types.)
   */
  def AnnotType       = rule( SimpleType ~ rep(NotNL ~ Annotation) )
  def AtomicType      = rule( WildcardType | ProductType | SingletonType | StableId )
  def CompoundType    = oneOrBoth(IntersectionType, Refinement)
  def ExistentialPart = rule( `forSome` ~ UnmodifiedDefs() )
  def FunArgTypes     = rule( InfixType | inParens(ParamTypeF0) )
  def FunctionType    = rule( FunArgTypes ~ RArrow ~ Type )
  def InfixType: R0   = rule( rep1sep(CompoundType, NotNL ~ Id ~ OneNLMax) )
  def ParamType: R0   = rule( Type ~ opt(Star) | RArrow ~ Type )
  def ParentType      = rule( AnnotType ~ rep(NotNL ~ ArgumentExprs) )
  def ProductType     = rule( '(' ~ repsep(ParamType, Comma) ~ ')' )
  def SimpleType      = rule( AtomicType ~ rep(TypeSuffix) )
  def SingletonType   = rule( StableId ~ Dot ~ `type` )
  def TypeArg         = rule( Type ~ TypeConstraints )
  def TypeArgs        = rule( '[' ~ TypeArg ~ rep(Comma ~ TypeArg) ~ ']' )
  def TypeParam: R0   = rule( IdOrUscore ~ TypeConstraints )
  def TypeProjection  = rule( Hash ~ Id )
  def TypeSuffix      = rule( TypeArgs | TypeProjection )
  def WildcardType    = rule( Uscore )

  /** Pattern rules.
   */
  def BindablePattern        = rule( opt(VarIdOrUscore ~ At) ~ ( WildcardStar | InfixPattern ) )
  def CaseClause: R0         = rule( `case` ~ Pattern ~ opt(InExpr.Guard) ~ RArrow ~ ImpliedBlock )
  def CaseBlock              = rule( '{' ~ rep1(CaseClause) ~ '}' )
  def ConstructorPattern     = rule( StableId ~ opt(TypeArgs) ~ opt(ProductPattern) )
  def InfixPattern           = rule( rep1sep(SimplePattern, Id) )
  def PatternAlternative: R0 = rule( TypedPattern | BindablePattern )
  def PatternAscription      = rule( Colon ~ CompoundType )
  def Patterns               = rule( rep1sep(BindablePattern ~ opt(RArrow ~ BindablePattern), Comma) )
  def ProductPattern         = rule( inParens(PatternF0) )
  def TypedPattern           = rule( VarIdOrUscore ~ PatternAscription )
  def UnderscorePattern      = rule( Uscore ~ !Star ~ opt(PatternAscription) )
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
    def DoExpr     = rule( `do`      ~!~ Expr ~ OptSemi ~ `while` ~ ParenExpr )
    def ElsePart   = rule( `else`    ~!~ Expr )
    def FinPart    = rule( `finally` ~!~ Expr )
    def ForExpr    = rule( `for`     ~!~ EnumeratorsPart ~ opt(`yield`) ~ Expr )
    def IfExpr     = rule( `if`      ~!~ ParenExpr ~ Expr ~ opt(OptSemi ~ ElsePart) )
    def Guard      = rule( `if`      ~!~ PostfixExpr )
    def MatchPart  = rule( `match`   ~!~ CaseBlock )
    def NewExpr    = rule( `new`     ~!~ ExtendsOrNew )
    def ReturnExpr = rule( `return`  ~!~ opt(Expr) )
    def ThrowExpr  = rule( `throw`   ~!~ Expr )
    def TryExpr    = rule( `try`     ~!~ Expr ~ ( `catch` ~ Expr ~ opt(FinPart) | FinPart | MATCH /** XXX scala bug - naked try **/ ) )
    def WhileExpr  = rule( `while`   ~!~ ParenExpr ~ Expr )

    def AssignPart  = rule( Equals ~ Expr )
    def InfixPart   = rule( NoNewlineInBlock ~ Id ~ opt(TypeArgs) ~ OptionalNewlineInBlock ~ PrefixExpr )
    def PostfixExpr = rule( PrefixExpr ~ rep(InfixPart) ~ opt(PostfixPart) )
    def PostfixPart = rule( NoNewlineInBlock ~ Id ~ opt(NL) ) // not OptNL!
    def PrefixExpr  = rule( opt(PrefixOperator) ~ SimpleExpr )
    def ProductExpr = rule( '(' ~ opt(Exprs) ~ ')' )

    /** For comprehensions.
     */
    def Enumerator      = rule( Generator | ForAssign | Guard )
    def Enumerators     = rule( Generator ~ rep(Semis ~ Enumerator) ~ WL )
    def ForAssign       = rule( opt(`val`) ~ PatternAlternative ~ Equals ~ Expr ~ opt(Guard) )  // Leading val is deprecated
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
    def LambdaExpr     = rule( rep1(LambdaArgs ~ RArrow) ~ ( Expr | ImpliedBlock ) )

    def Expr: R0 = rule(
        LambdaExpr
      | IfExpr
      | WhileExpr
      | TryExpr
      | DoExpr
      | ForExpr
      | ThrowExpr
      | ReturnExpr
      | PostfixExpr ~ opt( MatchPart | Ascription | AssignPart )
    )
  }

  def LambdaArgs = rule(
      inParens(NameAndOptType)
    | `implicit` ~ Id ~ OptInfixType
    | IdOrUscore ~ OptInfixType
  )

  def ArgumentExprs: R0 = rule(
      '(' ~ opt(Exprs ~ opt(VarargsStar)) ~ ')'
    | ExplicitBlock
  )
  def ValueConstraint = rule(
      Colon ~ ParamType
    | Equals ~ opt(`macro`) ~ ExprSensitive
  )
  def TypeConstraint   = rule(
      SubType ~ Type
    | SuperType ~ Type
    | ViewBound ~ Type
    | Colon ~ Type
    | Equals ~ Type
    | TypeTypeParamList
  )

  def AccessModifier       = rule( ( `private` | `protected` ) ~ opt(Qualifier) )
  def AnnotArgument: R0    = rule( opt(Id ~ Equals) ~ ( !(Id ~ Colon) ~ Expr ) )
  def Annotation           = rule( At ~ SimpleType ~ opt(NotNL ~ AnnotationArguments) ) // needs SimpleType to accept type arguments
  def AnnotationArguments  = rule( '(' ~ repsep(AnnotArgument, Comma) ~ ')' )
  def Annotations          = rule( rep(Annotation) )
  def AnnotationsAndMods   = rule( rep(Annotation ~ OneNLMax) ~ rep(Modifier) )
  def Ascription           = rule( Colon ~ ( WildcardStar | Type | ValueAnnotations ) )
  def Block: R0            = ImpliedBlock
  def ClassObjectOrTrait   = rule( `class` | `object` | `trait` )
  def ConstructorMods      = rule( rep(Annotation ~ NotNL) ~ rep(Modifier) )
  def EarlyDefs            = rule( inBraces(() => MemberDef) ~ `with` )
  def ExplicitBlock: R0    = rule( OneNLMax ~ ( CaseBlock | '{' ~ BlockStatSeq() ~ '}' ) )
  def ExprSensitive        = rule( InBlock.Expr )
  def Exprs: R0            = rule( rep1sep(WL ~ Expr, Comma) )
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
  def MethodTypeParamList  = inBrackets(AnnotatedTypeParam)
  def MethodTypeParamLists = rule( rep(MethodTypeParamList) )
  def Modifier             = rule( `abstract` | `final` | `sealed` | `implicit` | `lazy` | `override` | AccessModifier )
  def NamesAndType         = rule( rep1sep(Id, Comma) ~ Colon ~ Type )
  def OptEquals            = rule( opt(Equals ~ Expr) )
  def OptInfixType         = rule( opt(Colon ~ InfixType) )
  def OptParamType         = rule( opt(Colon ~ ParamType) )
  def OptType              = rule( opt(Colon ~ Type) )
  def PackageDef: R0       = rule( Package ~ QualId ~ inBraces(TopStat) )
  def ParenExpr            = rule( '(' ~ Expr ~ ')' )
  def SelfType: R0         = rule( IdOrUscoreOrThis ~ OptInfixType ~ RArrow )
  def TypeConstraints      = rule( rep(TypeConstraint) )
  def TypeTypeParamList    = inBrackets(VariantTypeParam)
  def TypeTypeParamLists   = rule( rep(TypeTypeParamList) )
  def ValueAnnotations     = rule( rep1(Annotation) )  // e.g. (x: @switch) match { ... }
  def ValueConstraints     = rule( rep(ValueConstraint) )
  def VarargsStar          = rule( Colon ~ WildcardStar )

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
    private def DefIntro          = rule( `def` ~ IdOrThis ~ MethodTypeParamLists ~ MethodParamLists )
    private def ConstructorParams = rule( opt(NotNL ~ ConstructorMods) ~ opt(MethodParamLists ~ !RArrow) )
    private def TemplateIntro     = rule( TemplateKeyword ~ Id ~ TypeTypeParamLists ~ ConstructorParams )
    private def DefBody           = rule( ExplicitBlock | ValueConstraints )
    private def EqualsBody        = rule( Equals ~ opt(`macro`) ~ ExprSensitive )
    private def ExtendsClause     = rule( ( `extends` | SubType ) ~ ExtendsOrNew )
    private def TemplateOpt       = rule( ExtendsClause | opt(Template()) )

    def DefDef      = rule( DefIntro ~ DefBody )
    def TemplateDef = rule( TemplateIntro ~ TemplateOpt )
    def TypeDef     = rule( `type` ~ Id ~ TypeConstraints )
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
