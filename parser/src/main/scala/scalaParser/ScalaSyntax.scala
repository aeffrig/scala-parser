package scalaParser

import syntax._
import org.parboiled2._
import scalaParser.macros.Macros._
import psp.std.ansi._

abstract class PspParser extends Parser with Basic with Identifiers with Literals {
  /**
   * Parses all whitespace, excluding newlines. This is only
   * really useful in e.g. {} blocks, where we want to avoid
   * capturing newlines so semicolon-inference would work
   */
  def WS = rule( rep(Basic.WhitespaceChar | Literals.Comment) )

  /**
   * Parses whitespace, including newlines.
   * This is the default for most things
   */
  def WL = rule( rep(Basic.WhitespaceChar | Literals.Comment | Basic.Newline) )
}

class ScalaSyntax(val input: ParserInput) extends PspParser with Keywords with Xml {

  /**
   * By default, all strings and characters greedily
   * capture all whitespace immediately before the token.
   */
  implicit private[this] def wspStr(s: String): R0 = rule( WL ~ str(s) )
  implicit private[this] def wspChar(s: Char): R0  = rule( WL ~ ch(s) )

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
  def Variance     = rule( "+" | "-" )
  def VBound       = rule( `<%` )

  /**
   * helper printing function
   */
  def pr(s: String) = rule { run(println(s"LOGGING $cursor: $s")) }

  def CommentWS        = rule( SpaceWS ~ Literals.Comment ~ SpaceWS ~ Basic.Newline )
  def Id               = rule( WL ~ Identifiers.Id )
  def IdDot            = rule( Id ~ Dot )
  def IdOrThis         = rule( Id | `this` )
  def IdOrThisOrSuper  = rule( Id | `this` | `super` )
  def IdOrUscore       = rule( Id | Uscore )
  def IdOrUscoreOrThis = rule( Id | Uscore | `this` )
  def Literal          = rule( WL ~ Literals.Literal )
  def NL               = rule( WL ~ Basic.Newline )
  def OptNL            = rule( WS ~ opt(Basic.Newline) )
  def QualId           = rule( WL ~ rep1sep(Id, Dot) )
  def QualSuper        = rule( `super` ~ opt(Qualifier) )
  def Semi             = rule( WS ~ Basic.Semi )
  def Semis            = rule( rep1(Semi) )
  def SpaceWS          = rule( rep(Basic.WhitespaceChar) )
  def ThisOrSuper      = rule(`this` | QualSuper )
  def VarId            = rule( WL ~ Identifiers.VarId )
  def VarIdOrUscore    = rule( VarId | Uscore )

  def Qualifier    = rule( '[' ~ IdOrThis ~ ']' )
  def StableId: R0 = rule( rep1sep(IdOrThisOrSuper, Dot) )

  def InfixType: R0    = rule( rep1sep(CompoundType, NotNL ~ Id ~ OneNLMax) )
  def CompoundType     = oneOrBoth(IntersectionType, rule( OneNLMax ~ inBraces(Unmodified.RefinementDcl) ) )
  def IntersectionType = rule( rep1sep(ParentType, `with`) )
  def ParentType       = rule( AnnotType ~ rep(NotNL ~ ArgumentExprs) )
  def AnnotType        = rule( SimpleType ~ rep(NotNL ~ Annotation) )
  def WildcardType     = rule( Uscore )
  def AtomicType       = rule( WildcardType | UnitType | ProductType | SingletonType | StableId )
  def SimpleType       = rule( AtomicType ~ TypeSuffix )

  def ValueAnnotations  = rule( rep1(Annotation) )  // e.g. (x: @switch) match { ... }
  def Ascription        = rule( Colon ~ ( WildcardStar | Type | ValueAnnotations ) )
  def Binding           = rule( IdOrUscore ~ OptType )
  def Bindings          = rule( repsep(Binding, Comma) )
  def Bindings1         = rule( rep1sep(Binding, Comma) )
  def ExistentialClause = rule( `forSome` ~ inBraces(Unmodified.Dcl) )
  def NotNL: R0         = rule( &( WS ~ !Basic.Newline ) )
  def OneNLMax: R0      = rule( OptNL ~ rep(CommentWS) ~ NotNL )
  def ProductType       = rule( '(' ~ rep1sep(ParamType, Comma) ~ ')' )
  def SingletonType     = rule( StableId ~ Dot ~ `type` )
  def TypeProjection    = rule( Hash ~ Id )
  def TypeSuffix        = rule( rep(TypeArgs | TypeProjection) )
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

    def AssignExpr = rule( NotSensitive.SimpleExpr ~ Equals ~ Expr )
    def DoExpr     = rule( `do` ~ Expr ~ optSemi ~ `while` ~ ParenExpr )
    def ForExpr    = rule( `for` ~ EnumeratorsPart ~ opt(`yield`) ~ Expr )
    def IfExpr     = rule( `if` ~ ParenExpr ~ Expr ~ opt(optSemi ~ ElsePart) )
    def ReturnExpr = rule( `return` ~ opt(Expr) )
    def ThrowExpr  = rule( `throw` ~ Expr )
    def TryExpr    = rule( TryPart ~ opt(CatchPart) ~ opt(FinPart) )
    def TupleExpr  = rule( '(' ~ opt(Exprs) ~ ')' )
    def WhileExpr  = rule( `while` ~ ParenExpr ~ Expr )

    def TryPart   = rule( `try` ~ Expr )
    def CatchPart = rule( `catch` ~ Expr )
    def FinPart   = rule( `finally` ~ Expr )
    def ElsePart  = rule( `else` ~ Expr )
    def MatchPart = rule( `match` ~ CaseBlock )

    def Enumerators     = rule( Generator ~ rep(Semis ~ Enumerator) ~ WL )
    def Generator: R0   = rule( Pattern1 ~ LArrow ~ Expr ~ opt(Guard) )
    def ForAssignment   = rule( Pattern1 ~ Equals ~ Expr ~ opt(Guard) )
    def Guard: R0       = rule( `if` ~ PostfixExpr )
    def InfixPart       = rule( MaybeNotNL ~ Id ~ opt(TypeArgs) ~ MaybeOneNewline ~ PrefixExpr )
    def PostfixExpr: R0 = rule( PrefixExpr ~ rep(InfixPart) ~ opt(PostfixPart) )
    def PostfixPart     = rule( MaybeNotNL ~ Id ~ opt(NL) )
    def PrefixExpr      = rule( opt(PrefixOpchar) ~ SimpleExpr )
    def PrefixOpchar    = rule( WL ~ anyOf("-+~!") ~ WS ~ !Basic.OperatorChar )

    def SimpleExprStart = rule(
        XmlExpr
      | NewExpr
      | BlockExpr
      | Literal
      | StableId
      | Uscore
      | TupleExpr
    )
    def SimpleExprPart = rule(
        Dot ~ Id
      | TypeArgs
      | MaybeNotNL ~ ArgumentExprs
    )
    def SimpleExpr: R0 = rule( SimpleExprStart ~ rep(SimpleExprPart) ~ opt(MaybeNotNL ~ Uscore) )

    def Enumerator: R0 = rule(
        Generator
      | Guard
      | opt(`val` /*deprecated*/) ~ ForAssignment
    )
    def Expr: R0 = rule(
      rep(LambdaHead) ~ (
          IfExpr
        | WhileExpr
        | TryExpr
        | DoExpr
        | ForExpr
        | ThrowExpr
        | ReturnExpr
        | AssignExpr
        | PostfixExpr ~ opt(MatchPart | Ascription)
      )
    )
  }

  def LambdaHead = rule(
    (   inParens(Binding)
      | opt(`implicit`) ~ Id ~ OptInfixType
      | Uscore ~ opt(Ascription)
    ) ~ RArrow
  )

  def ArgumentExprs: R0 = rule(
      '(' ~ opt(Exprs ~ opt(VarargsStar)) ~ ')'
    | OneNLMax ~ BlockExpr
  )

  def Block: R0 = {
    def ResultExpr: R0 = rule( ExprSensitive | LambdaHead ~ Block )

    rule(
      rep(LambdaHead) ~ optSemis ~ (
          ResultExpr ~ BlockEnd
        | repsep(BlockStat, Semis) ~ opt(Semis ~ ResultExpr) ~ BlockEnd
      )
    )
  }

  def EmptyParens  = rule( '(' ~ ')' )
  def ParamClause  = rule( '(' ~ ( `implicit` ~ Params1 | Params ) ~ ')' )
  def ParamClauses = rule( rep(ParamClause) )
  def Params       = rule( repsep(Param, Comma) )
  def Params1      = rule( rep1sep(Param, Comma) )
  def Param        = rule( Annotations ~ opt(rep(Modifier) ~ ValOrVar) ~ Id ~ OptParamType ~ OptEquals )

  def Pattern            = AltPattern
  def PatternType        = rule( CompoundType )
  def PatternAscription  = rule( Colon ~ PatternType )
  def UnderscorePattern  = rule( Uscore ~ !Star ~ opt(PatternAscription) )
  def VariablePattern    = rule( VarId )
  def TypedPattern       = rule( VarIdOrUscore ~ PatternAscription )
  def PatternBinding     = rule( VarIdOrUscore ~ At )
  def Pattern1: R0       = rule( TypedPattern | Pattern2 )
  def Pattern2           = rule( opt(PatternBinding) ~ ( WildcardStar | InfixPattern ) )
  def AltPattern         = rule( rep1sep(Pattern1, Pipe) )
  def InfixPattern       = rule( rep1sep(SimplePattern, Id) )
  def ConstructorPattern = rule( StableId ~ opt(ProductPattern) )
  def ProductPattern     = rule( inParens(Pattern) )
  def LiteralPattern     = rule( Literal )

  def SimplePattern = rule(
      XmlPattern
    | UnderscorePattern
    | LiteralPattern
    | ProductPattern
    | ConstructorPattern
    | VariablePattern
  )

  def FunTypeParamClause     = inBrackets(rule(Annotations ~ TypeParam))
  def VariantTypeParamClause = inBrackets(rule(Annotations ~ opt(WL ~ Variance) ~ TypeParam))
  def TypeParamClauses       = rule( rep(VariantTypeParamClause) )
  def FunTypeParamClauses    = rule( rep(FunTypeParamClause) )

  def TypeParam: R0    = rule( IdOrUscore ~ TypeParamClauses ~ MethodTypeBounds )
  def TypeBounds       = rule( opt(SuperType ~ Type) ~ opt(SubType ~ Type) )
  def MethodTypeBounds = rule( TypeBounds ~ ViewBounds ~ ContextBounds )
  def ContextBounds    = rule( rep(Colon ~ Type) )
  def ViewBounds       = rule( rep(VBound ~ Type) )

  /** Highest level rules. */
  def Dcl      = rule( AnnotationsAndMods ~ Unmodified.Dcl )
  def Def      = rule( AnnotationsAndMods ~ Unmodified.Def )
  def Type: R0 = rule( rep1sep(InfixType, RArrow) ~ TypeBounds ~ opt(ExistentialClause) )
  def Expr     = rule( NotSensitive.Expr )
  def CompUnit = rule( semiSeparated(FlatPackageStat) ~ TopStatSeq ~ WL )

  def CompilationUnit: Rule1[String] = rule( capture(CompUnit) ~ EOI )

  def AccessModifier     = rule( ( `private` | `protected` ) ~ opt(Qualifier) )
  def Annotation         = rule( At ~ SimpleType ~ rep(ArgumentExprs) )
  def Annotations        = rule( rep(Annotation) )
  def AnnotationsAndMods = rule( rep(Annotation ~ OneNLMax) ~ rep(Modifier) )
  def BlockBody          = rule( OneNLMax ~ '{' ~ Block ~ '}' )
  def BlockExpr: R0      = rule( '{' ~ (CaseClauses | Block) ~ optSemis ~ '}' )
  def CaseBlock          = rule( '{' ~ CaseClauses ~ '}' )
  def CaseClause: R0     = rule( `case` ~ Pattern ~ opt(NotSensitive.Guard) ~ RArrow ~ Block )
  def CaseClauses: R0    = rule( rep1(CaseClause) )
  def ConstructorMods    = rule( NotNL ~ AnnotationsAndMods )
  def EarlyDefs          = rule( inBraces(Def) ~ `with` )
  def ExprSensitive      = rule( IsSensitive.Expr )
  def Exprs: R0          = rule( rep1sep(Expr, Comma) )
  def ExtendsClause      = rule( `extends` ~ ExtendsOrNew )
  def ExtendsOrNew       = oneOrBoth(Parents, Template)
  def FlatPackageStat    = rule( Package ~ QualId ~ !BlockStart )
  def Import             = rule( `import` ~ ImportExprs )
  def ImportExpr         = rule( StableId ~ opt(ImportSuffix) )
  def ImportExprs        = rule( rep1sep(ImportExpr, Comma) )
  def ImportSelector     = rule( IdOrUscore ~ opt(RArrow ~ IdOrUscore) )
  def ImportSelectors    = rule( '{' ~ rep1sep(ImportSelector, Comma) ~ '}' )
  def ImportSuffix       = rule( Dot ~ (Uscore | ImportSelectors) )
  def Modifier           = rule( `abstract` | `final` | `sealed` | `implicit` | `lazy` | `override` | AccessModifier )
  def NamesAndType       = rule( rep1sep(Id, Comma) ~ Colon ~ Type )
  def NewExpr            = rule( `new` ~ ExtendsOrNew )
  def OptEquals          = rule( opt(Equals ~ Expr) )
  def OptInfixType       = rule( opt(Colon ~ InfixType) )
  def OptType            = rule( opt(Colon ~ Type) )
  def OptParamType       = rule( opt(Colon ~ ParamType) )
  def ParamType: R0      = rule( Type ~ opt(Star) | RArrow ~ Type )
  def PackageDef         = rule( Package ~ QualId ~ inBraces(TopStat) )
  def ParenExpr          = rule( '(' ~ Expr ~ ')' )
  def Parents            = rule( opt(EarlyDefs) ~ IntersectionType )
  def Patterns           = rule( rep1sep(Pattern2, Comma) )
  def SelfType: R0       = rule( IdOrUscoreOrThis ~ OptInfixType ~ RArrow )
  def Template           = rule( '{' ~ opt(SelfType) ~ semiSeparated(TemplateStat) ~ '}' )
  def TemplateDef        = rule( AnnotationsAndMods ~ Unmodified.TemplateDef )
  def TemplateOpt        = rule( ExtendsClause | opt(Template) )
  def TemplateStat: R0   = rule( BlockStat | Dcl )
  def TopStatSeq         = semiSeparated(TopStat)
  def TypeArgs           = inBrackets(Type)
  def ValOrVar           = rule( `val` | `var` )
  def VarargsStar        = rule( Colon ~ WildcardStar )

  def optSemi  = rule( opt(Semi) )
  def optSemis = rule( opt(Semis) )

  def BlockStat: R0  = rule(
      Import
    | Def
    | ExprSensitive
  )

  def errorContextWidth: Int            = 1
  def errorCharMarkup(ch: Char): String = {
    import scala.Console._
    RED + BOLD + REVERSED + ch + RESET
  }

  override def errorTraceCollectionLimit = 6
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
    (line - errorContextWidth) to (line + errorContextWidth) flatMap line_s mkString "\n"
  }

  /** Recombinators. */
  def oneOrBoth(p: => R0, q: => R0): R0 = rule( p ~ opt(q) | q )
  def inBraces(stat: => R0): R0         = rule( '{' ~ semiSeparated(stat) ~ '}' )
  def inParens(elem: => R0): R0         = rule( '(' ~ repsep(elem, Comma) ~ ')' )
  def inBrackets(param: => R0): R0      = rule( '[' ~ rep1sep(param, Comma) ~ ']' )
  def semiSeparated(stat: => R0): R0    = rule( optSemis ~ repsep(stat, Semis) ~ optSemis )

  object Unmodified {
    private def FunDefIntro      = rule( `def` ~ IdOrThis ~ FunTypeParamClauses ~ ParamClauses )
    private def TypeDefIntro     = rule( `type` ~ Id ~ TypeParamClauses )
    private def TemplateDefIntro = rule( TemplateKeyword ~ Id ~ TypeParamClauses ~ opt(ConstructorMods) ~ ParamClauses )
    private def FunBody          = rule( OptType ~ EqualsBody | BlockBody )
    private def EqualsBody       = rule( Equals ~ opt(`macro`) ~ ExprSensitive )

    def TypeAliasDef        = rule( TypeDefIntro ~ Equals ~ Type )
    def TemplateDef         = rule( TemplateDefIntro ~ TemplateOpt )
    def PatternDef          = rule( ValOrVar ~ Patterns ~ OptType ~ EqualsBody )
    def UninitializedVarDef = rule( `var` ~ NamesAndType ~ Equals ~ Uscore )
    def FunDef              = rule( FunDefIntro ~ FunBody )

    def Def: R0 = rule(
        UninitializedVarDef
      | PatternDef
      | TypeAliasDef
      | FunDef
      | TemplateDef
    )
    def Dcl = rule(
        TypeDefIntro ~ TypeBounds
      | ValOrVar ~ NamesAndType
      | FunDefIntro ~ OptType
    )
    def RefinementDcl = rule( TypeAliasDef | Dcl )
  }

  def TemplateKeyword = rule(
      `trait`
    | `class`
    | `object`
    | `case` ~ `class`
    | `case` ~ `object`
    | `package` ~ `object`
  )
  def TopStat: R0 = rule(
      Import
    | PackageDef
    | TemplateDef
  )

  private def BlockStart = rule( &( WS ~ '{' ) )
  private def BlockEnd   = rule( optSemis ~ &( '}' | `case` ) )
}
