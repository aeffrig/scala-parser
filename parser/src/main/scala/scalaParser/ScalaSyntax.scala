package scalaParser

import syntax._
import org.parboiled2._
import scalaParser.macros.Macros._

/**
 * Parser for Scala syntax.
 */
class ScalaSyntax(val input: ParserInput) extends Parser with Basic with Identifiers with Literals with Keywords with Xml {
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
  def SubType       = rule( `<:` )
  def SuperType     = rule( `>:` )
  def Uscore        = rule( `_` )
  def WildcardStar  = rule( `_` ~ `*` )

  /**
   * helper printing function
   */
  def pr(s: String) = rule { run(println(s"LOGGING $cursor: $s")) }

  def CommentWS        = rule( SpaceWS ~ Literals.Comment ~ SpaceWS ~ Basic.Newline )
  def DotId            = rule( Dot ~ Id )
  def Id               = rule( WL ~ Identifiers.Id )
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

  private def InfixTypeRest = rule( ArrowType | opt(ExistentialClause) )
  private def TypeStart     = rule(
      Uscore
    | FunctionArgTypes ~ ArrowType
    | InfixType ~ InfixTypeRest
  )

  def AnnotType         = rule( SimpleType ~ opt(NotNL ~ rep1(NotNL ~ Annotation)) )
  def ArrowType         = rule( RArrow ~ Type )
  def Ascription        = rule( Colon ~ ( WildcardStar | Type | Annotations ) )
  def Binding           = rule( IdOrUscore ~ OptType )
  def CompoundType      = rule( AnnotType ~ WithClauses ~ opt(Refinement) | Refinement )
  def ExistentialClause = rule( `forSome` ~ '{' ~ rep1sep(StableDcl, Semi) ~ '}' )
  def FunctionArgTypes  = rule( '(' ~ repsep(ParamType, Comma) ~ ')' )
  def InfixType         = rule( CompoundType ~ rep(NotNL ~ Id ~ OneNLMax ~ CompoundType) )
  def NotNL: R0         = rule( &( WS ~ !Basic.Newline ) )
  def OneNLMax: R0      = rule( OptNL ~ rep(CommentWS) ~ NotNL )
  def ParamType         = rule( ArrowType | RepeatedType | Type )
  def ProductType       = rule( '(' ~ Types ~ ')' )
  def Refinement        = rule( OneNLMax ~ '{'  ~ RefinementStatSeq ~ '}' )
  def RefinementStat    = rule( `type` ~ TypeDef | Dcl | MATCH )
  def RepeatedType      = rule( Type ~ `*` )
  def SimpleType        = rule( ( ProductType | SingletonType | StableId ) ~ TypeSuffix )
  def SingletonType     = rule( StableId ~ Dot ~ `type` )
  def Type: R0          = rule( TypeStart ~ TypeBounds )
  def TypeArgs          = rule( '[' ~ Types ~ ']' )
  def TypeProjection    = rule( '#' ~ Id )
  def TypeSuffix        = rule( rep(TypeArgs | TypeProjection) )
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
    (   '(' ~ repsep(Binding, Comma) ~ ')'
      | opt(`implicit`) ~ Id ~ OptInfixType
      | Uscore ~ opt(Ascription)
    ) ~ RArrow
  )

  def ArgumentExprs: R0 = rule(
      '(' ~ opt(Exprs ~ opt(VarargsStar)) ~ ')'
    | OneNLMax ~ BlockExpr
  )

  def BlockStats: R0 = {
    def Template: R0 = rule( rep(Annotation) ~ (opt(`implicit`) ~ opt(`lazy`) ~ Def | rep(LocalModifier) ~ TmplDef) )
    def BlockStat: R0 = rule(
        Import
      | Template
      | ExprSensitive
    )
    rule( rep1sep(BlockStat, Semis) )
  }

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

  def Pattern        = rule( rep1sep(Pattern1, Pipe) )
  def Pattern1: R0   = rule( Uscore ~ Colon ~ TypePat | VarId ~ Colon ~ TypePat | Pattern2 )
  def Pattern2       = rule( VarId ~ At ~ Pattern3 | Pattern3 | VarId )
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

  def AccessModifier      = rule( (`private` | `protected`) ~ opt(AccessQualifier) )
  def AccessQualifier     = rule( '[' ~ (`this` | Id) ~ ']' )
  def AnnotatedTypeParams = rule( rep1sep(rep(Annotation) ~ TypeParam, Comma) )
  def Annotation          = rule( At ~ SimpleType ~ rep(ArgumentExprs) )
  def Annotations         = rule( rep1(Annotation) )
  def BlockExpr: R0       = rule( '{' ~ (CaseClauses | Block) ~ optSemis ~ '}' )
  def CaseBlock           = rule( '{' ~ CaseClauses ~ '}' )
  def CaseClause: R0      = rule( `case` ~ Pattern ~ opt(NotSensitive.Guard) ~ RArrow ~ Block )
  def CaseClauses: R0     = rule( rep1(CaseClause) )
  def ClassParam          = rule( rep(Annotation) ~ opt(Modifiers ~ ValOrVar) ~ Id ~ Colon ~ ParamType ~ opt(Equals ~ Expr) )
  def Expr                = rule( NotSensitive.Expr )
  def ExprSensitive       = rule( IsSensitive.Expr )
  def Exprs: R0           = rule( rep1sep(Expr, Comma) )
  def FunTypeParamClause  = rule( '[' ~ AnnotatedTypeParams ~ ']' )
  def HighBound           = rule( SubType ~ Type )
  def ImplicitParamClause = rule( OneNLMax ~ '(' ~ `implicit` ~ Params ~ ')' )
  def Import              = rule( `import` ~ ImportExprs )
  def ImportExpr          = rule( StableId ~ opt(ImportSuffix) )
  def ImportExprs         = rule( rep1sep(ImportExpr, Comma) )
  def ImportRename        = rule( RArrow ~ IdOrUscore )
  def ImportSelector      = rule( Id ~ opt(ImportRename) )
  def ImportSelectors     = rule( '{' ~ rep(ImportSelector ~ Comma) ~ (ImportSelector | Uscore) ~ '}' )
  def ImportSuffix        = rule( Dot ~ (Uscore | ImportSelectors) )
  def LocalModifier       = rule( `abstract` | `final` | `sealed` | `implicit` | `lazy` )
  def LowBound            = rule( SuperType ~ Type )
  def MethodTypeBounds    = rule( TypeBounds ~ rep(ViewBound) ~ rep(Colon ~ Type) )
  def Modifier            = rule( LocalModifier | AccessModifier | `override` )
  def Modifiers           = rule( rep(Modifier) )
  def OptType             = rule( opt(Colon ~ Type) )
  def OptInfixType        = rule( opt(Colon ~ InfixType) )
  def Param               = rule( rep(Annotation) ~ Id ~ opt(Colon ~ ParamType) ~ opt(Equals ~ Expr) )
  def ParamClause         = rule( OneNLMax ~ '(' ~ opt(Params) ~ ')' )
  def ParamClauses        = rule( rep(ParamClause) ~ opt(ImplicitParamClause) )
  def Params              = rule( repsep(Param, Comma) )
  def ParenExpr           = rule( '(' ~ Expr ~ ')' )
  def SelfInvocation: R0  = rule( `this` ~ rep1(ArgumentExprs) )
  def TypeBounds          = rule( opt(LowBound) ~ opt(HighBound) )
  def TypeDef             = rule( Id ~ TypeParamClauses ~ Equals ~ Type )
  def TypeParam: R0       = rule( IdOrUscore ~ TypeParamClauses ~ MethodTypeBounds )
  def TypeParamClause     = rule( '[' ~ VariantTypeParams ~ ']' )
  def TypeParamClauses    = rule( rep(TypeParamClause) )
  def ValOrVar            = rule( `val` | `var` )
  def VarargsStar         = rule( Colon ~ WildcardStar )
  def VarianceAnnot       = rule( WL ~ anyOf("+-") )
  def VariantTypeParam    = rule( rep(Annotation) ~ opt(VarianceAnnot) ~ TypeParam )
  def VariantTypeParams   = rule( rep1sep(VariantTypeParam, Comma) )
  def ViewBound           = rule( `<%` ~ Type )
  def WithClauses         = rule( rep(`with` ~ AnnotType) )

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

  def BlockBody            = rule( OneNLMax ~ '{' ~ Block ~ '}' )
  def ClassDef             = rule( Id ~ TypeParamClauses ~ opt(NotNL ~ ConstrPrelude) ~ opt(ClassParamClauses) ~ ClassTemplateOpt )
  def ClassParamClause     = rule( OneNLMax ~ '(' ~ opt(ClassParams) ~ ')' )
  def ClassParamClauses    = rule( rep1(ClassParamClause) ~ opt(ImplicitClause) | ImplicitClause )
  def ClassParams          = rule( rep1sep(ClassParam, Comma) )
  def ClassTemplate        = rule( opt(EarlyDefs) ~ Parents ~ opt(TemplateBody) )
  def ClassTemplateOrBody  = rule( ClassTemplate | TemplateBody )
  def ConstrPrelude        = rule( Annotations ~ opt(AccessModifier) | opt(Annotations) ~ AccessModifier )
  def ConstructorBlockBody = rule( '{' ~ SelfInvocation ~ opt(Semis ~ BlockStats) ~ optSemis ~ '}' )
  def ConstructorBody      = rule( ConstructorBlockBody | SelfInvocation )
  def EarlyDef             = rule( rep(Annotation ~ OneNLMax) ~ Modifiers ~ EarlyableDef )
  def EarlyDefs            = rule( '{' ~ repsep(EarlyDef, Semis) ~ optSemis ~ '}' ~ `with` )
  def FlatPackageStat      = rule( Package ~ QualId ~ !BlockStart )
  def FunSig               = rule( Id ~ opt(FunTypeParamClause) ~ ParamClauses )
  def ImplicitClause       = rule( OneNLMax ~ '(' ~ `implicit` ~ ClassParams ~ ')' )
  def NamesAndType         = rule( rep1sep(Id, Comma) ~ Colon ~ Type )
  def NewExpr              = rule( `new` ~ ClassTemplateOrBody )
  def ObjectDef            = rule( Id ~ ClassTemplateOpt )
  def Parents              = rule( AnnotType ~ rep(NotNL ~ ArgumentExprs) ~ WithClauses )
  def Patterns             = rule( rep1sep(Pattern2, Comma) )
  def TemplateBody         = rule( '{' ~ opt(SelfType) ~ TemplateStatSeq ~ '}' )
  def TmplDef              = rule( TemplateDefIntro ~ ClassDef )

  def TopPackageSeq     = rule( optSemis ~ repsep(FlatPackageStat, Semis) ~ optSemis )
  def TemplateStatSeq   = rule( optSemis ~ repsep(TemplateStat, Semis) ~ optSemis )
  def TopStatSeq        = rule( optSemis ~ repsep(TopStat, Semis) ~ optSemis )
  def RefinementStatSeq = rule( optSemis ~ repsep(RefinementStat, Semis) ~ optSemis )
  def CompUnit          = rule( TopPackageSeq ~ TopStatSeq ~ WL )

  def CompilationUnit: Rule1[String] = rule( capture(CompUnit) )

  def EarlyableDef = rule(
      `var` ~ NamesAndType ~ Equals ~ Uscore
    | ValOrVar ~ Patterns ~ OptType ~ Equals ~ ExprSensitive
    | `type` ~ TypeDef
  )
  def Def: R0 = rule(
      EarlyableDef
    | `def` ~ `this` ~ ParamClause ~ ParamClauses ~ (Equals ~ ConstructorBody | OneNLMax ~ ConstructorBlockBody)
    | `def` ~ FunSig ~ OptType ~ Equals ~ opt(`macro`) ~ ExprSensitive
    | `def` ~ FunSig ~ BlockBody
    | TmplDef
  )
  def StableDcl = rule(
      `val` ~ NamesAndType
    | `type` ~ Id ~ TypeParamClauses ~ TypeBounds
  )
  def Dcl = rule(
      StableDcl
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
      Package ~ QualId ~ '{' ~ TopStatSeq ~ '}'
    | PackageObject ~ ObjectDef
    | Import
    | rep(Annotation ~ OneNLMax) ~ Modifiers ~ TmplDef
  )

  private def BlockStart = rule( &( WS ~ '{' ) )
  private def BlockEnd   = rule( optSemis ~ &( '}' | `case` ) )
}
