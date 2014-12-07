package scalaParser

import syntax._
import org.parboiled2._
import scalaParser.macros.Macros._
import psp.std.ansi._

abstract class PspParser extends Parser with Basic with Identifiers with Literals {
  def input: ParserInput

  def prevN(n: Int): String = input.sliceString(cursor - n, cursor)
  def nextN(n: Int): String = input.sliceString(cursor, cursor + n)

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

class TraceScalaSyntax(in: ParserInput) extends ScalaSyntax(in) {
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
  // println("\nCreated tracer")
  // override implicit def wspStr(s: String): R0 = rule( WL ~ str(s) )
  // override implicit def wspChar(s: Char): R0  = rule( WL ~ ch(s) ) //~ pr("`%s`" format s) )

  override def Expr = rule( super.Expr ~ run(tick()) )
  override def Type = rule( super.Type ~ run(tick()) )
}

class ScalaSyntax(val input: ParserInput) extends PspParser with Keywords with Xml {
  /** This is horrific but seems necessary in current parboiled.
   */
  val AnnotatedTypeParam : F0R0 = () => rule( Annotations ~ TypeParam )
  val BlockStat          : F0R0 = () => rule( Import | Definition | ExprSensitive )
  val EarlyDef           : F0R0 = () => rule( Definition )
  val ExistentialStat    : F0R0 = () => rule( Unmodified.Dcl )
  val FlatPackageStat    : F0R0 = () => rule( Package ~ QualId ~ !BlockStart )
  val IntersectionType   : F0R0 = () => rule( rep1sep(ParentType, `with`) )
  val NameAndOptType     : F0R0 = () => rule( IdOrUscore ~ OptType )
  val RefinementStat     : F0R0 = () => rule( Unmodified.Dcl )
  val TopStat            : F0R0 = () => rule( Import | PackageDef | TemplateDef )
  val VariantTypeParam   : F0R0 = () => rule( Annotations ~ opt(WL ~ Variance) ~ TypeParam )

  val Refinement      : F0R0 = () => rule( OneNLMax ~ inBraces(RefinementStat) )
  val TemplateStat    : F0R0 = () => rule( BlockStat() | Declaration )

  val BlockStatSeq    : F0R0 = () => semiSeparated(BlockStat)
  val TopStatSeq      : F0R0 = () => semiSeparated(TopStat)
  val TemplateStatSeq : F0R0 = () => semiSeparated(TemplateStat)
  val PackageStatSeq  : F0R0 = () => semiSeparated(FlatPackageStat)

  val ParamTypeF0 = () => ParamType
  val ParentsF0   = () => Parents
  val PatternF0   = () => Pattern
  val TemplateF0  = () => Template
  val TypeArgF0   = () => TypeArg

  /**
   * By default, all strings and characters greedily
   * capture all whitespace immediately before the token.
   */
  implicit def wspStr(s: String): R0 = rule( WL ~ str(s) )
  implicit def wspChar(s: Char): R0  = rule( WL ~ ch(s) )

  def At           = rule( `@` )
  def Colon        = rule( `:` )
  def Comma        = rule( ',' )
  def Dot          = rule( '.' )
  def Hash         = rule( `#` )
  def Equals       = rule( `=` )
  def LArrow       = rule( `<-` )
  def Package      = rule( `package` )
  def Pipe         = rule( `|` )
  def RArrow       = rule( `=>` )
  def Star         = rule( `*` )
  def SubType      = rule( `<:` )
  def SuperType    = rule( `>:` )
  def Uscore       = rule( `_` )
  def WildcardStar = rule( Uscore ~ Star )
  def Variance     = rule( Plus | Minus )
  def VBound       = rule( `<%` )

  /**
   * helper printing function
   */
  def pr(s: String) = rule( run(println("%4s  %s".format(cursor, s))) )

  def CommentWS        = rule( SpaceWS ~ Literals.Comment ~ SpaceWS ~ Basic.Newline )
  def Id               = rule( WL ~ Identifiers.Id )
  def IdDot            = rule( Id ~ Dot )
  def IdOrThis         = rule( Id | This )
  def IdOrUscore       = rule( Id | Uscore )
  def IdOrUscoreOrThis = rule( Id | Uscore | This )
  def NL               = rule( WL ~ Basic.Newline )
  def OptNL            = rule( WS ~ opt(Basic.Newline) )
  def QualId           = rule( WL ~ rep1sep(Id, Dot) )
  def QualSuper        = rule( `super` ~ opt(Qualifier) )
  def Semi             = rule( WS ~ Basic.Semi )
  def Semis            = rule( rep1(Semi) )
  def SpaceWS          = rule( rep(Basic.WhitespaceChar) )
  def This             = rule( `this` )
  def ThisOrSuper      = rule( This | QualSuper )
  def VarId            = rule( WL ~ Identifiers.VarId )
  def VarIdOrUscore    = rule( VarId | Uscore )

  def Qualifier    = rule( '[' ~ IdOrThis ~ ']' )
  def StableId: R0 = rule( rep1sep(Id | This | QualSuper, Dot) )

  def InfixType: R0    = rule( rep1sep(CompoundType, NotNL ~ Id ~ OneNLMax) )
  def CompoundType     = oneOrBoth(IntersectionType, Refinement)
  def ParentType       = rule( AnnotType ~ rep(NotNL ~ ArgumentExprs) )
  def AnnotType        = rule( SimpleType ~ rep(NotNL ~ Annotation) )
  def WildcardType     = rule( Uscore )
  def AtomicType       = rule( WildcardType | UnitType | ProductType | SingletonType | StableId )
  def SimpleType       = rule( AtomicType ~ rep(TypeSuffix) )

  def ValueAnnotations  = rule( rep1(Annotation) )  // e.g. (x: @switch) match { ... }
  def Ascription        = rule( Colon ~ ( WildcardStar | Type | ValueAnnotations ) )
  def ExistentialClause = rule( `forSome` ~ inBraces(ExistentialStat) )
  def NotNL: R0         = rule( &( WS ~ !Basic.Newline ) )
  def OneNLMax: R0      = rule( OptNL ~ rep(CommentWS) ~ NotNL )
  def ProductType       = rule( '(' ~ rep1sep(ParamType, Comma) ~ ')' )
  def SingletonType     = rule( StableId ~ Dot ~ `type` )
  def TypeProjection    = rule( Hash ~ Id )
  def TypeSuffix        = rule( TypeArgs | TypeProjection )
  def UnitType          = rule( EmptyParens )

  def EnumeratorsPart = rule(
      '(' ~ NotSensitive.Enumerators ~ ')'
    | '{' ~ IsSensitive.Enumerators ~ '}'
  )

  object IsSensitive extends SensitiveRules(semicolonInference = true)
  object NotSensitive extends SensitiveRules(semicolonInference = false)

  abstract class SensitiveRules(semicolonInference: Boolean) {
    def MaybeOneNewline: R0 = if (semicolonInference) OneNLMax else MATCH
    def MaybeNotNL: R0      = if (semicolonInference) NotNL else MATCH

    def DoExpr     = rule( `do` ~ Expr ~ optSemi ~ `while` ~ ParenExpr )
    def ForExpr    = rule( `for` ~ EnumeratorsPart ~ opt(`yield`) ~ Expr )
    def IfExpr     = rule( `if` ~ ParenExpr ~ Expr ~ opt(optSemi ~ ElsePart) )
    def NewExpr    = rule( `new` ~ ExtendsOrNew )
    def ReturnExpr = rule( `return` ~ opt(Expr) )
    def ThrowExpr  = rule( `throw` ~ Expr )
    def TryExpr    = rule( `try` ~ Expr ~ ( `catch` ~ Expr ~ opt(FinPart) | FinPart | MATCH /** XXX scala bug - naked try **/ ) )
    def TupleExpr  = rule( '(' ~ opt(Exprs) ~ ')' )
    def WhileExpr  = rule( `while` ~ ParenExpr ~ Expr )

    def FinPart    = rule( `finally` ~ Expr )
    def ElsePart   = rule( `else` ~ Expr )
    def MatchPart  = rule( `match` ~ '{' ~ CaseClauses ~ '}' )
    def AssignPart = rule( Equals ~ Expr )

    def Enumerators     = rule( Generator ~ rep(Semis ~ Enumerator) ~ WL )
    def Generator: R0   = rule( PatternAlternative ~ LArrow ~ Expr ~ opt(Guard) )
    def ForAssignment   = rule( PatternAlternative ~ Equals ~ Expr ~ opt(Guard) )
    def Guard: R0       = rule( `if` ~ PostfixExpr )
    def InfixPart       = rule( MaybeNotNL ~ Id ~ opt(TypeArgs ~ pr("InfixPart")) ~ MaybeOneNewline ~ PrefixExpr )
    def PostfixExpr: R0 = rule( PrefixExpr ~ rep(InfixPart) ~ opt(PostfixPart) )
    def PostfixPart     = rule( MaybeNotNL ~ Id ~ opt(NL) ) // not OptNL!
    def PrefixExpr      = rule( opt(PrefixOperator) ~ SimpleExpr )

    def SimpleExprStart = rule(
        XmlExpr
      | NewExpr
      | ExplicitBlock
      | TupleExpr
      | Literals.Expr
      | StableId
      | Uscore
    )
    def SimpleExprPart = rule(
        TypeArgs
      | Dot ~ Id
      | MaybeNotNL ~ ArgumentExprs
    )
    def SimpleExpr: R0 = rule( SimpleExprStart ~ rep(SimpleExprPart) ~ opt(MaybeNotNL ~ Uscore) )

    def Enumerator: R0 = rule(
        Generator
      | Guard
      | opt(`val` /*deprecated*/) ~ ForAssignment
    )
    def LambdaExpr = rule( rep1(LambdaArgs ~ RArrow) ~ ( Expr | ImpliedBlock ) )

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
  def Block: R0         = ImpliedBlock
  def ImpliedBlock: R0  = rule( optSemis ~ BlockStatSeq() ~ BlockEnd )
  def ExplicitBlock: R0 = rule( OneNLMax ~ '{' ~ ( CaseClauses | BlockStatSeq() ) ~ '}' )

  // def ClassLiteral        = rule( "classOf" ~ '[' ~ StableId ~ ']' )
  // def ArrayLiteral        = rule( "Array" ~ '(' ~ repsep(AnnotArgument, Comma) ~ ')' )
  // def AnnotLiteral        = rule( Literals.Expr | ClassLiteral | ArrayLiteral | StableId )

  def AnnotArgument: R0   = rule( opt(Id ~ Equals) ~ ( !(Id ~ Colon) ~ Expr ) )
  def AnnotationArguments = rule( '(' ~ repsep(AnnotArgument, Comma) ~ ')' )

  def ArgumentExprs: R0 = rule(
      '(' ~ opt(Exprs ~ opt(VarargsStar)) ~ ')'
    | ExplicitBlock
  )

  def EmptyParens  = rule( '(' ~ ')' )
  def ParamClause  = rule( '(' ~ ( `implicit` ~ Params1 | Params ) ~ ')' )
  def ParamClauses = rule( rep(ParamClause) )
  def Params       = rule( repsep(Param, Comma) )
  def Params1      = rule( rep1sep(Param, Comma) )
  def Param        = rule( AnnotationsAndMods ~ opt(ValOrVar) ~ Id ~ rep(ValueConstraint) )

  def Pattern                = AltPattern
  def PatternType            = rule( CompoundType )
  def PatternAscription      = rule( Colon ~ PatternType )
  def UnderscorePattern      = rule( Uscore ~ !Star ~ opt(PatternAscription) )
  def VariablePattern        = rule( VarId )
  def TypedPattern           = rule( VarIdOrUscore ~ PatternAscription )
  def PatternBinding         = rule( VarIdOrUscore ~ At )
  def PatternAlternative: R0 = rule( TypedPattern | BindablePattern )
  def BindablePattern        = rule( opt(PatternBinding) ~ ( WildcardStar | InfixPattern ) )
  def AltPattern             = rule( rep1sep(PatternAlternative, Pipe) )
  def InfixPattern           = rule( rep1sep(SimplePattern, Id) )
  def ConstructorPattern     = rule( StableId ~ opt(TypeArgs) ~ opt(ProductPattern) )
  def ProductPattern         = rule( inParens(PatternF0) )

  def SimplePattern = rule(
      XmlPattern
    | UnderscorePattern
    | Literals.Pattern
    | ProductPattern
    | ConstructorPattern
    | VariablePattern
  )

  def TypeTypeParamList  = inBrackets(VariantTypeParam)
  def TypeTypeParamLists = rule( rep(TypeTypeParamList) )

  def MethodTypeParamList  = inBrackets(AnnotatedTypeParam)
  def MethodTypeParamLists = rule( rep(MethodTypeParamList) )

  // In scala it would suffice for TypeBounds to follow underscore, since the only
  // place you can use it is e.g. Foo[_ <: Bar]
  //
  // but we want to be able to do other things such as
  //   case x: Foo[t <: Bar] => (x: t) => 1
  //
  def TypeArg          = rule( Type ~ TypeConstraints )
  def TypeParam: R0    = rule( IdOrUscore ~ TypeTypeParamLists ~ TypeConstraints )

  def ValueConstraint = rule(
      Colon ~ ParamType
    | Equals ~ opt(`macro`) ~ ExprSensitive
  )
  def ValueConstraints = rule( rep(ValueConstraint) )
  def TypeConstraints  = rule( rep(TypeConstraint) )
  def TypeConstraint   = rule(
      SubType ~ Type
    | SuperType ~ Type
    | VBound ~ Type
    | Colon ~ Type
    | Equals ~ Type
  )

  /** Highest level rules. */
  def Declaration  = rule( AnnotationsAndMods ~ Unmodified.Dcl )
  def Definition   = rule( AnnotationsAndMods ~ Unmodified.Def )
  def Type: R0     = rule( InfixType ~ opt(ExistentialClause | RArrow ~ Type) )
  def FunArgTypes  = rule( InfixType | inParens(ParamTypeF0) )
  def Expr         = rule( NotSensitive.Expr )
  def CompUnit     = rule( PackageStatSeq() ~ TopStatSeq() ~ WL )
  def FunctionType = rule( FunArgTypes ~ RArrow ~ Type )

  def CompilationUnit: Rule1[String] = rule( capture(CompUnit) ~ EOI )

  def AccessModifier     = rule( ( `private` | `protected` ) ~ opt(Qualifier) )
  def Annotation         = rule( At ~ SimpleType ~ opt(NotNL ~ AnnotationArguments) ) // needs SimpleType to accept type arguments
  def Annotations        = rule( rep(Annotation) )
  def AnnotationsAndMods = rule( rep(Annotation ~ OneNLMax) ~ rep(Modifier) )
  def CaseClause: R0     = rule( `case` ~ Pattern ~ opt(NotSensitive.Guard) ~ RArrow ~ ImpliedBlock )
  def CaseClauses: R0    = rule( rep1(CaseClause) )
  def ConstructorMods    = rule( rep(Annotation ~ NotNL) ~ rep(Modifier) )
  def EarlyDefs          = rule( inBraces(EarlyDef) ~ `with` )
  def ExprSensitive      = rule( IsSensitive.Expr )
  def Exprs: R0          = rule( rep1sep(WL ~ Expr, Comma) )
  def ExtendsOrNew       = oneOrBoth(ParentsF0, TemplateF0)
  def Import             = rule( `import` ~ ImportExprs )
  def ImportExpr         = rule( StableId ~ opt(ImportSuffix) )
  def ImportExprs        = rule( rep1sep(ImportExpr, Comma) )
  def ImportSelector     = rule( IdOrUscore ~ opt(RArrow ~ IdOrUscore) )
  def ImportSelectors    = rule( '{' ~ rep1sep(ImportSelector, Comma) ~ '}' )
  def ImportSuffix       = rule( Dot ~ (Uscore | ImportSelectors) )
  def Modifier           = rule( `abstract` | `final` | `sealed` | `implicit` | `lazy` | `override` | AccessModifier )
  def NamesAndType       = rule( rep1sep(Id, Comma) ~ Colon ~ Type )
  def OptEquals          = rule( opt(Equals ~ Expr) )
  def OptInfixType       = rule( opt(Colon ~ InfixType) )
  def OptType            = rule( opt(Colon ~ Type) )
  def OptParamType       = rule( opt(Colon ~ ParamType) )
  def ParamType: R0      = rule( Type ~ opt(Star) | RArrow ~ Type )
  def PackageDef: R0     = rule( Package ~ QualId ~ inBraces(TopStat) )
  def ParenExpr          = rule( '(' ~ Expr ~ ')' )
  def Parents            = rule( opt(EarlyDefs) ~ IntersectionType() )
  def Patterns           = rule( rep1sep(BindablePattern ~ opt(RArrow ~ BindablePattern), Comma) )
  def SelfType: R0       = rule( IdOrUscoreOrThis ~ OptInfixType ~ RArrow )
  def Template           = rule( '{' ~ opt(SelfType) ~ TemplateStatSeq() ~ '}' )
  def TemplateDef        = rule( AnnotationsAndMods ~ Unmodified.TemplateDef )
  def TypeArgs           = rule( '[' ~ TypeArg ~ rep(Comma ~ TypeArg) ~ ']' )
  def ValOrVar           = rule( `val` | `var` )
  def VarargsStar        = rule( Colon ~ WildcardStar )

  def optSemi  = rule( opt(Semi) )
  def optSemis = rule( opt(Semis) )

  def errorContextWidth: Int            = 1
  def errorCharMarkup(ch: Char): String = {
    import scala.Console._
    RED + BOLD + REVERSED + ch + RESET
  }

  // override def errorTraceCollectionLimit = 6
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
  def semiSeparated(stat: F0R0): R0   = rule( optSemis ~ repsep(stat(), Semis) ~ optSemis )

  object Unmodified {
    // The !RArrow in TemplateParams is defense against "class A" and "() => 5" on consecutive lines.
    private def DefDefIntro     = rule( `def` ~ IdOrThis ~ MethodTypeParamLists ~ ParamClauses )
    // private def TypeDefIntro = rule( `type` ~ Id ~ TypeParamClauses )
    private def ValueParamLists = rule( opt(NotNL ~ ConstructorMods) ~ opt(ParamClauses ~ !RArrow) )
    private def TemplateIntro   = rule( TemplateKeyword ~ Id ~ TypeTypeParamLists ~ ValueParamLists )
    private def DefBody         = rule( ExplicitBlock | ValueConstraints )
    private def EqualsBody      = rule( Equals ~ opt(`macro`) ~ ExprSensitive )
    private def ExtendsClause   = rule( ( `extends` | SubType ) ~ ExtendsOrNew )
    private def TemplateOpt     = rule( ExtendsClause | opt(Template) )

    def TemplateDef = rule( TemplateIntro ~ TemplateOpt )
    def ValDef      = rule( ValOrVar ~ Patterns ~ ValueConstraints )
    def TypeDef     = rule( `type` ~ Id ~ TypeTypeParamLists ~ TypeConstraints )
    def DefDef      = rule( DefDefIntro ~ DefBody )

    def TemplateKeyword = rule(
        `trait`
      | `class`
      | `object`
      | `case` ~ `object`
      | `case` ~ `class`
      | `package` ~ `object`
    )
    def Def: R0 = rule(
        ValDef
      | TypeDef
      | DefDef
      | TemplateDef
    )
    def MemberDef   = rule(
        ValOrVar ~ Patterns ~ ValueConstraints
      | `type` ~ Id ~ TypeTypeParamLists ~ TypeConstraints
      | DefDefIntro ~ DefBody
      | TemplateDef
    )
    def Dcl = MemberDef

    // def Dcl = rule(
    //     TypeDefIntro ~ TypeConstraints
    //   | ValOrVar ~ NamesAndType
    //   | FunDefIntro ~ OptType
    // )

    // The !RArrow in TemplateParams is defense against "class A" and "() => 5" on consecutive lines.
    // private def FunDefIntro    = rule( `def` ~ IdOrThis ~ FunTypeParamClauses ~ ParamClauses )
    // private def TypeDefIntro   = rule( `type` ~ Id ~ TypeParamClauses )
    // private def TemplateParams = rule( opt(NotNL ~ ConstructorMods) ~ opt(ParamClauses ~ !RArrow) )
    // private def TemplateIntro  = rule( TemplateKeyword ~ Id ~ TypeParamClauses ~ TemplateParams )
    // private def FunBody        = rule( ExplicitBlock | rep(ValueConstraint) )
    // private def EqualsBody     = rule( Equals ~ opt(`macro`) ~ ExprSensitive )
    // private def ExtendsClause  = rule( ( `extends` | SubType ) ~ ExtendsOrNew )
    // private def TemplateOpt    = rule( ExtendsClause | opt(Template) )
  }

  private def BlockStart = rule( &( WS ~ '{' ) )
  private def BlockEnd   = rule( optSemis ~ &( '}' | `case` ) )
}
