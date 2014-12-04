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

  def At            = rule( `@` )
  def Colon         = rule( `:` )
  def Comma         = rule( ',' )
  def Dot           = rule( '.' )
  def Equals        = rule( `=` )
  def LArrow        = rule( `<-` )
  def Package       = rule( `package` )
  def PackageObject = rule( `package` ~ `object` )
  def Pipe          = rule( `|` )
  def RArrow        = rule( `=>` )
  def Star          = rule( `*` )
  def SubType       = rule( `<:` )
  def SuperType     = rule( `>:` )
  def Uscore        = rule( `_` )
  def WildcardStar  = rule( Uscore ~ Star )

  /**
   * helper printing function
   */
  def pr(s: String) = rule { run(println(s"LOGGING $cursor: $s")) }

  def CommentWS        = rule( SpaceWS ~ Literals.Comment ~ SpaceWS ~ Basic.Newline )
  def DotId            = rule( Dot ~ Id )
  def Id               = rule( WL ~ Identifiers.Id )
  def IdOrThis         = rule( Id | `this` )
  def IdDot            = rule( Id ~ Dot )
  def IdOrUscore       = rule( Id | Uscore )
  def IdOrUscoreOrThis = rule( Id | Uscore | `this` )
  def Literal          = rule( WL ~ Literals.Literal )
  def NL               = rule( WL ~ Basic.Newline )
  def OptNL            = rule( WS ~ opt(Basic.Newline) )
  def QualId           = rule( WL ~ rep1sep(Id, Dot) )
  def Semi             = rule( WS ~ Basic.Semi )
  def Semis            = rule( rep1(Semi) )
  def SpaceWS          = rule( rep(Basic.WhitespaceChar) )
  def VarId            = rule( WL ~ Identifiers.VarId )
  def VarIdOrUscore    = rule( VarId | Uscore )

  def ThisOrSuper = rule(
      `this`
    | `super` ~ opt(ClassQualifier)
  )

  def ClassQualifier = rule( '[' ~ Id ~ ']' )
  def StableId: R0 = rule(
      rep(IdDot) ~ ThisOrSuper ~ rep(DotId)
    | rep1sep(Id, Dot)
  )

  // private def TypeStart = rule(
  //     Uscore
  //   | FunctionType
  //   | ArrowsType ~ opt(ExistentialClause)
  // )
  // def Type: R0 = rule(
  //     RArrow ~ Type
  //   | Uscore ~ TypeEnd
  //   | FunctionType ~ TypeEnd
  //   | rep1sep(InfixType, RArrow) ~ TypeEnd
  // )

  def Type: R0   = rule( TypeStart ~ TypeMiddle ~ TypeEnd )
  def TypeStart  = rule( opt(RArrow) )
  def TypeMiddle = rule( rep1sep(WildcardType | InfixType, RArrow) )
  def TypeEnd    = rule( TypeBounds ~ opt(ExistentialClause) ~ opt(Star) )

  def InfixType        = rule( rep1sep(CompoundType, NotNL ~ Id ~ OneNLMax) )
  def ParentType       = rule( AnnotType ~ rep(NotNL ~ ArgumentExprs) )
  def IntersectionType = rule( rep1sep(ParentType, `with`) )
  def CompoundType     = oneOrBoth(IntersectionType, rule( OneNLMax ~ Refinement ) )
  def Refinement       = inBraces(RefinementStat)
  def AnnotType        = rule( SimpleType ~ rep(NotNL ~ Annotation) )

  def FunctionType      = rule( inParens(Type) ~ RArrow ~ Type )
  def ArrowType         = rule( RArrow ~ Type )
  def Ascription        = rule( Colon ~ ( WildcardStar | Type | Annotations1 ) )
  def Binding           = rule( IdOrUscore ~ OptType )
  def Bindings          = rule( repsep(Binding, Comma) )
  def Bindings1         = rule( rep1sep(Binding, Comma) )
  def ExistentialClause = rule( `forSome` ~ inBraces(Dcl) )
  def NotNL: R0         = rule( &( WS ~ !Basic.Newline ) )
  def OneNLMax: R0      = rule( OptNL ~ rep(CommentWS) ~ NotNL )
  def ProductType       = rule( '(' ~ Types ~ ')' )
  def RefinementStat    = rule( `type` ~ TypeDefAlias | Dcl | MATCH )
  def SimpleType        = rule( ( UnitType | ProductType | SingletonType | StableId ) ~ TypeSuffix )
  def SingletonType     = rule( StableId ~ Dot ~ `type` )
  def TypeArgs          = rule( '[' ~ Types ~ ']' )
  def TypeProjection    = rule( '#' ~ Id )
  def TypeSuffix        = rule( rep(TypeArgs | TypeProjection) )
  def UnitType          = rule( EmptyParens )
  def WildcardType      = rule( Uscore )
  def Types             = rule( rep1sep(Type, Comma) )

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
    def ExprTrailer     = rule( MatchPart | Ascription )
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
      | Path
      | Uscore
      | TupleExpr
    )
    def SimpleExprPart = rule(
        DotId
      | TypeArgs
      | MaybeNotNL ~ ArgumentExprs
    )
    def SimpleExpr: R0 = rule( SimpleExprStart ~ rep(SimpleExprPart) ~ opt(MaybeNotNL ~ Uscore) )

    def Path: R0 = rule(
        rep(IdDot) ~ `this` ~ rep(DotId)
      | StableId
    )
    def Enumerator: R0 = rule(
        Generator
      | Guard
      | ForAssignment
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
        | PostfixExpr ~ opt(ExprTrailer)
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

  def BlockStats: R0 = rule( rep1sep(BlockStat, Semis) )
  def BlockStat: R0  = rule(
      Import
    | Annotations ~ rep(LocalModifier) ~ Def
    | ExprSensitive
  )

  def Block: R0 = {
    def ResultExpr: R0 = rule( ExprSensitive | LambdaHead ~ Block )

    rule(
      rep(LambdaHead) ~ optSemis ~ (
          ResultExpr ~ BlockEnd
        | BlockStats ~ opt(Semis ~ ResultExpr) ~ BlockEnd
        | MATCH ~ BlockEnd
      )
    )
  }

  def EmptyParens      = rule( '(' ~ ')' )
  def ClassParams      = rule( repsep(ClassParam, Comma) )
  def ClassParams1     = rule( rep1sep(ClassParam, Comma) )
  def ClassParamClause = rule( '(' ~ ( `implicit` ~ ClassParams1 | ClassParams ) ~ ')' )
  def ParamClause      = rule( '(' ~ ( `implicit` ~ Params1 | Params ) ~ ')' )
  def ParamClauses     = rule( rep(ParamClause) )
  def Params           = rule( repsep(Param, Comma) )
  def Params1          = rule( rep1sep(Param, Comma) )
  def Param            = rule( Annotations ~ Id ~ OptType ~ opt(Equals ~ Expr) )
  def ClassParam       = rule( Annotations ~ opt(Modifiers ~ ValOrVar) ~ Id ~ Colon ~ Type ~ opt(Equals ~ Expr) )

  def Pattern        = rule( rep1sep(Pattern1, Pipe) )
  def Pattern1: R0   = rule( Uscore ~ Colon ~ TypePat | VarId ~ Colon ~ TypePat | Pattern2 )
  def Pattern2       = rule( VarIdOrUscore ~ At ~ Pattern3 | Pattern3 | VarId )
  def Pattern3       = rule( WildcardStar | rep1sep(SimplePattern, Id) )
  def ExtractorArgs  = rule( repsep(Pattern, Comma) )
  def Extractor      = rule( StableId ~ opt(TuplePattern) )
  def TuplePattern   = rule( '(' ~ ExtractorArgs ~ ')' )
  def TypePat        = rule( CompoundType )
  def TypedPattern   = rule( !WildcardStar ~ Uscore ~ opt(Colon ~ TypePat) )
  def LiteralPattern = rule( Literal )
  def SimplePattern  = rule(
      XmlPattern
    | TypedPattern
    | LiteralPattern
    | TuplePattern
    | Extractor
    | VarId
  )

  def InvariantTypeParamClause = inBrackets(rule(Annotations ~ TypeParam))
  def VariantTypeParamClause   = inBrackets(rule(Annotations ~ opt(VarianceAnnot) ~ TypeParam))
  def TypeParamClauses         = rule( rep(VariantTypeParamClause) )

  def TypeDefAlias    = rule( Id ~ TypeParamClauses ~ Equals ~ Type )
  def TypeDefAbstract = rule( Id ~ TypeParamClauses ~ TypeBounds )
  def TypeParam: R0   = rule( IdOrUscore ~ TypeParamClauses ~ MethodTypeBounds )

  def ContextBounds = rule( rep(Colon ~ Type) )
  def ViewBounds    = rule( rep(ViewBound) )

  def BlockExpr: R0      = rule( '{' ~ (CaseClauses | Block) ~ optSemis ~ '}' )
  def CaseBlock          = rule( '{' ~ CaseClauses ~ '}' )
  def CaseClause: R0     = rule( `case` ~ Pattern ~ opt(NotSensitive.Guard) ~ RArrow ~ Block )
  def CaseClauses: R0    = rule( rep1(CaseClause) )
  def AccessModifier     = rule( (`private` | `protected`) ~ opt(AccessQualifier) )
  def AccessQualifier    = rule( '[' ~ (`this` | Id) ~ ']' )
  def Annotation         = rule( At ~ SimpleType ~ rep(ArgumentExprs) )
  def AnnotationsAndMods = rule( rep(Annotation ~ OneNLMax) ~ Modifiers )
  def Annotations        = rule( rep(Annotation) )
  def Annotations1       = rule( rep1(Annotation) )
  def Expr               = rule( NotSensitive.Expr )
  def ExprSensitive      = rule( IsSensitive.Expr )
  def Exprs: R0          = rule( rep1sep(Expr, Comma) )
  def HighBound          = rule( SubType ~ Type )
  def Import             = rule( `import` ~ ImportExprs )
  def ImportExpr         = rule( StableId ~ opt(ImportSuffix) )
  def ImportExprs        = rule( rep1sep(ImportExpr, Comma) )
  def ImportRename       = rule( RArrow ~ IdOrUscore )
  def ImportSelector     = rule( Id ~ opt(ImportRename) )
  def ImportSelectors    = rule( '{' ~ rep(ImportSelector ~ Comma) ~ (ImportSelector | Uscore) ~ '}' )
  def ImportSuffix       = rule( Dot ~ (Uscore | ImportSelectors) )
  def LocalModifier      = rule( `abstract` | `final` | `sealed` | `implicit` | `lazy` )
  def LowBound           = rule( SuperType ~ Type )
  def MethodTypeBounds   = rule( TypeBounds ~ ViewBounds ~ ContextBounds )
  def Modifier           = rule( LocalModifier | AccessModifier | `override` )
  def Modifiers          = rule( rep(Modifier) )
  def OptType            = rule( opt(Colon ~ Type) )
  def OptInfixType       = rule( opt(Colon ~ InfixType) )
  def ParenExpr          = rule( '(' ~ Expr ~ ')' )
  def SelfInvocation: R0 = rule( `this` ~ rep1(ArgumentExprs) )
  def TypeBounds         = rule( opt(LowBound) ~ opt(HighBound) )
  def ValOrVar           = rule( `val` | `var` )
  def VarargsStar        = rule( Colon ~ WildcardStar )
  def VarianceAnnot      = rule( WL ~ anyOf("+-") )
  def ViewBound          = rule( `<%` ~ Type )

  def optSemi  = rule( opt(Semi) )
  def optSemis = rule( opt(Semis) )

  def TemplateStat: R0 = rule(
      Import
    | rep(Annotation ~ OneNLMax) ~ Modifiers ~ ( Def | Dcl )
    | ExprSensitive
  )

  def SelfType: R0  = rule(
      `this` ~ Colon ~ InfixType ~ RArrow
    | IdOrUscore ~ OptInfixType ~ RArrow
  )

  def BlockBody           = rule( OneNLMax ~ '{' ~ Block ~ '}' )
  def ClassDef            = rule( Id ~ TypeParamClauses ~ opt(NotNL ~ AnnotationsAndMods) ~ rep(ClassParamClause) ~ ClassTemplateOpt )
  def ClassTemplate       = rule( opt(EarlyDefs ~ `with`) ~ IntersectionType ~ opt(TemplateBody) )
  def ClassTemplateOrBody = rule( ClassTemplate | TemplateBody )
  def EarlyDef            = rule( AnnotationsAndMods ~ EarlyableDef )
  def EarlyDefs           = inBraces(EarlyDef)
  def FlatPackageStat     = rule( Package ~ QualId ~ !BlockStart )
  def FunSig              = rule( IdOrThis ~ rep(InvariantTypeParamClause) ~ ParamClauses )
  def NamesAndType        = rule( rep1sep(Id, Comma) ~ Colon ~ Type )
  def NewExpr             = rule( `new` ~ ClassTemplateOrBody )
  def ObjectDef           = rule( Id ~ ClassTemplateOpt )
  def Patterns            = rule( rep1sep(Pattern2, Comma) )
  def TemplateBody        = rule( '{' ~ opt(SelfType) ~ semiSeparated(TemplateStat) ~ '}' )
  def TmplDef             = rule( TemplateDefIntro ~ ClassDef )

  def oneOrBoth(p: => R0, q: => R0): R0 = rule( p ~ opt(q) | q )
  def inBraces(stat: => R0): R0         = rule( '{' ~ semiSeparated(stat) ~ '}' )
  def inParens(elem: => R0): R0         = rule( '(' ~ repsep(elem, Comma) ~ ')' )
  def inBrackets(param: => R0): R0      = rule( '[' ~ rep1sep(param, Comma) ~ ']' )
  def semiSeparated(stat: => R0): R0    = rule( optSemis ~ repsep(stat, Semis) ~ optSemis )

  def TopStatSeq = semiSeparated(TopStat)
  def CompUnit   = rule( semiSeparated(FlatPackageStat) ~ TopStatSeq ~ WL )

  def CompilationUnit: Rule1[String] = rule( capture(CompUnit) ~ EOI )

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

  def EarlyableDef = rule(
      `var` ~ NamesAndType ~ Equals ~ Uscore
    | ValOrVar ~ Patterns ~ OptType ~ Equals ~ ExprSensitive
    | `type` ~ TypeDefAlias
  )
  def Def: R0 = rule(
      EarlyableDef
    | `def` ~ FunSig ~ FunBody
    | TmplDef
  )
  def EqualsBody = rule( Equals ~ opt(`macro`) ~ ExprSensitive )
  def FunBody = rule(
      OptType ~ EqualsBody
    | BlockBody
  )
  def Dcls1 = rule( rep1sep(Dcl, Semi) )
  def Dcl   = rule(
      `val` ~ NamesAndType
    | `type` ~ TypeDefAbstract
    | `var` ~ NamesAndType
    | `def` ~ FunSig ~ OptType
  )
  def TemplateDefIntro = rule(
      `trait`
    | `class`
    | `object`
    | `case` ~ `class`
    | `case` ~ `object`
    | PackageObject
  )

  def ClassTemplateOpt: R0 = rule(
      `extends` ~ ClassTemplateOrBody
    | TemplateBody
    | MATCH
  )

  private def TopStat: R0 = rule(
      Package ~ QualId ~ inBraces(TopStat)
    | PackageObject ~ ObjectDef
    | Import
    | AnnotationsAndMods ~ TmplDef
  )

  private def BlockStart = rule( &( WS ~ '{' ) )
  private def BlockEnd   = rule( optSemis ~ &( '}' | `case` ) )
}
