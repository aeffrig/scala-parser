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

  def Comma        = rule( ',' )
  def At           = rule( `@` )
  def Colon        = rule( `:` )
  def LArrow       = rule( `<-` )
  def RArrow       = rule( `=>` )
  def SubType      = rule( `<:` )
  def SuperType    = rule( `>:` )
  def Uscore       = rule( `_` )
  def WildcardStar = rule( `_` ~ `*` )
  def Pipe         = rule( `|` )

  /**
   * helper printing function
   */
  def pr(s: String) = rule { run(println(s"LOGGING $cursor: $s")) }

  def CommentWS        = rule( SpaceWS ~ Literals.Comment ~ SpaceWS ~ Basic.Newline )
  def DotId            = rule( '.' ~ Id )
  def Id               = rule( WL ~ Identifiers.Id )
  def IdDot            = rule( Id ~ '.' )
  def IdOrUscore       = rule( Id | Uscore )
  def IdOrUscoreOrThis = rule( Id | Uscore | `this` )
  def Literal          = rule( WL ~ Literals.Literal )
  def NL               = rule( WL ~ Basic.Newline )
  def OptNL            = rule( WS ~ opt(Basic.Newline) )
  def QualId           = rule( WL ~ rep1sep(Id, '.') )
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
    | rep1sep(Id, '.')
  )

  private def InfixTypeRest = rule( ArrowType | opt(ExistentialClause) )
  private def TypeStart     = rule(
      Uscore
    | FunctionArgTypes ~ ArrowType
    | InfixType ~ InfixTypeRest
  )

  def CompoundType = {
    def RefineStat     = rule( `type` ~ TypeDef | Dcl | MATCH )
    def optRefineStats = rule( repsep(RefineStat, Semis) )
    def Refinement     = rule( OneNLMax ~ '{'  ~ optSemis ~ optRefineStats ~ optSemis ~ '}' )
    rule(
        AnnotType ~ WithClauses ~ opt(Refinement)
      | Refinement
    )
  }

  def AnnotType         = rule( SimpleType ~ opt(NotNL ~ rep1(NotNL ~ Annotation)) )
  def ArrowType         = rule( RArrow ~ Type )
  def Ascription        = rule( Colon ~ ( WildcardStar | Type | Annotations ) )
  def Binding           = rule( IdOrUscore ~ OptType )
  def ColonInfixType    = rule( Colon ~ InfixType )
  def ColonParamType    = rule( Colon ~ ParamType )
  def ExistentialClause = rule( `forSome` ~ '{' ~ rep1sep(StableDcl, Semi) ~ '}' )
  def FunctionArgTypes  = rule( '(' ~ repsep(ParamType, Comma) ~ ')' )
  def InfixType         = rule( CompoundType ~ rep(NotNL ~ Id ~ OneNLMax ~ CompoundType) )
  def NotNL: R0         = rule( &( WS ~ !Basic.Newline ) )
  def OneNLMax: R0      = rule( OptNL ~ rep(CommentWS) ~ NotNL )
  def ParamType         = rule( ArrowType | RepeatedType | Type )
  def ProductType       = rule( '(' ~ Types ~ ')' )
  def RepeatedType      = rule( Type ~ `*` )
  def SimpleType        = rule( ( ProductType | SingletonType | StableId ) ~ TypeSuffix )
  def SingletonType     = rule( StableId ~ '.' ~ `type` )
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

    def AssignExpr = rule( NotSensitive.SimpleExpr ~ `=` ~ Expr )
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
    def ForAssignment   = rule( Pattern1 ~ `=` ~ Expr ~ opt(Guard) )
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
      | opt(`implicit`) ~ Id ~ opt(Colon ~ InfixType)
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

  def Modifiers   = rule( rep(Modifier) )
  def Annotations = rule( rep1(Annotation) )

  def AccessModifier      = rule( (`private` | `protected`) ~ opt(AccessQualifier) )
  def AccessQualifier     = rule( '[' ~ (`this` | Id) ~ ']' )
  def MethodTypeBounds    = rule( TypeBounds ~ rep(ViewBound) ~ rep(Colon ~ Type) )
  def AnnotatedTypeParams = rule( rep1sep(rep(Annotation) ~ TypeParam, Comma) )
  def Annotation          = rule( At ~ SimpleType ~ rep(ArgumentExprs) )
  def BlockExpr: R0       = rule( '{' ~ (CaseClauses | Block) ~ optSemis ~ '}' )
  def CaseBlock           = rule( '{' ~ CaseClauses ~ '}' )
  def CaseClause: R0      = rule( `case` ~ Pattern ~ opt(NotSensitive.Guard) ~ RArrow ~ Block )
  def CaseClauses: R0     = rule( rep1(CaseClause) )
  def ClassParam          = rule( rep(Annotation) ~ opt(Modifiers ~ ValOrVar) ~ Id ~ ColonParamType ~ opt(`=` ~ Expr) )
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
  def ImportSuffix        = rule( '.' ~ (Uscore | ImportSelectors) )
  def LocalModifier       = rule( `abstract` | `final` | `sealed` | `implicit` | `lazy` )
  def LowBound            = rule( SuperType ~ Type )
  def Modifier            = rule( LocalModifier | AccessModifier | `override` )
  def Param               = rule( rep(Annotation) ~ Id ~ opt(ColonParamType) ~ opt(`=` ~ Expr) )
  def ParamClause         = rule( OneNLMax ~ '(' ~ opt(Params) ~ ')' )
  def ParamClauses        = rule( rep(ParamClause) ~ opt(ImplicitParamClause) )
  def Params              = rule( repsep(Param, Comma) )
  def ParenExpr           = rule( '(' ~ Expr ~ ')' )
  def SelfInvocation: R0  = rule( `this` ~ rep1(ArgumentExprs) )
  def TemplateStats       = rule( optSemis ~ repsep(TemplateStat, Semis) ~ optSemis )
  def TypeBounds          = rule( opt(LowBound) ~ opt(HighBound) )
  def TypeDef             = rule( Id ~ TypeParamClauses ~ `=` ~ Type )
  def TypeParam: R0       = rule( IdOrUscore ~ TypeParamClauses ~ MethodTypeBounds )
  def TypeParamClause     = rule( '[' ~ VariantTypeParams ~ ']' )
  def TypeParamClauses    = rule( rep(TypeParamClause) )
  def VarargsStar         = rule( Colon ~ WildcardStar )
  def VarianceAnnot       = rule( WL ~ anyOf("+-") )
  def VariantTypeParam    = rule( rep(Annotation) ~ opt(VarianceAnnot) ~ TypeParam )
  def VariantTypeParams   = rule( rep1sep(VariantTypeParam, Comma) )
  def ViewBound           = rule( `<%` ~ Type )

  def optSemi  = rule( opt(Semi) )
  def optSemis = rule( opt(Semis) )
  def OptType  = rule( opt(Colon ~ Type) )

  private def ValOrVar          = rule( `val` | `var` )
  private def WithClauses       = rule( rep(`with` ~ AnnotType) )
  private def PackageObject: R0 = rule( `package` ~ `object` ~ ObjectDef )
  private def Packaging: R0     = rule( `package` ~ QualId ~ '{' ~ optSemis ~ opt(TopStatSeq) ~ optSemis ~ '}' )

  def TemplateStat: R0 = rule(
      Import
    | rep(Annotation ~ OneNLMax) ~ Modifiers ~ ( Def | Dcl )
    | ExprSensitive
  )

  def SelfType: R0  = rule(
      `this` ~ Colon ~ InfixType ~ RArrow
    | IdOrUscore ~ opt(Colon ~ InfixType) ~ RArrow
  )

  def ConstructorBody      = rule( ConstructorBlockBody | SelfInvocation )
  def FunSig               = rule( Id ~ opt(FunTypeParamClause) ~ ParamClauses )
  def Patterns             = rule( rep1sep(Pattern2, Comma) )
  def BlockBody            = rule( OneNLMax ~ '{' ~ Block ~ '}' )
  def ConstructorBlockBody = rule( '{' ~ SelfInvocation ~ opt(Semis ~ BlockStats) ~ optSemis ~ '}' )
  def EarlyDef: R0         = rule( rep(Annotation ~ OneNLMax) ~ Modifiers ~ EarlyableDef )
  def EarlyDefs: R0        = rule( '{' ~ repsep(EarlyDef, Semis) ~ optSemis ~ '}' ~ `with` )

  def NamesAndType = rule( rep1sep(Id, Comma) ~ Colon ~ Type )
  def EarlyableDef = rule(
      `var` ~ NamesAndType ~ `=` ~ Uscore
    | ValOrVar ~ Patterns ~ OptType ~ `=` ~ ExprSensitive
    | `type` ~ TypeDef
  )

  def Def: R0 = rule(
      EarlyableDef
    | `def` ~ `this` ~ ParamClause ~ ParamClauses ~ (`=` ~ ConstructorBody | OneNLMax ~ ConstructorBlockBody)
    | `def` ~ FunSig ~ OptType ~ `=` ~ opt(`macro`) ~ ExprSensitive
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
    | `package` ~ `object`
  )

  def TmplDef: R0 = {
    def ClassParams           = rule( rep1sep(ClassParam, Comma) )
    def ImplicitClause: R0    = rule( OneNLMax ~ '(' ~ `implicit` ~ ClassParams ~ ')' )
    def ClassParamClause: R0  = rule( OneNLMax ~ '(' ~ opt(ClassParams) ~ ')' )
    def ClassParamClauses: R0 = rule( rep1(ClassParamClause) ~ opt(ImplicitClause) | ImplicitClause )
    def ConstrPrelude: R0     = rule( Annotations ~ opt(AccessModifier) | opt(Annotations) ~ AccessModifier )
    def ClassDef: R0          = rule( Id ~ TypeParamClauses ~ opt(NotNL ~ ConstrPrelude) ~ opt(ClassParamClauses) ~ ClassTemplateOpt )

    rule( TemplateDefIntro ~ ClassDef )
  }

  def ObjectDef: R0        = rule( Id ~ ClassTemplateOpt )
  def ClassTemplateOrBody  = rule( ClassTemplate | TemplateBody )
  def NewExpr              = rule( `new` ~ ClassTemplateOrBody )
  def TemplateBody: R0     = rule( '{' ~ opt(SelfType) ~ TemplateStats ~ '}' )
  def Parents              = rule( AnnotType ~ rep(NotNL ~ ArgumentExprs) ~ WithClauses )
  def ClassTemplate        = rule( opt(EarlyDefs) ~ Parents ~ opt(TemplateBody) )
  def ClassTemplateOpt: R0 = rule(
      `extends` ~ ClassTemplateOrBody
    | TemplateBody
    | MATCH
  )

  private def TopStat: R0 = rule(
      Packaging
    | PackageObject
    | Import
    | rep(Annotation ~ OneNLMax) ~ Modifiers ~ TmplDef
  )

  private def BlockStart = rule( &( WS ~ '{' ) )
  private def BlockEnd   = rule( optSemis ~ &( '}' | `case` ) )

  def FlatPackageStat    = rule( `package` ~ QualId ~ !BlockStart )
  def TopPackageSeq: R0  = rule( repsep(FlatPackageStat, Semis) )
  def TopStatSeq: R0     = rule( repsep(TopStat, Semis) )

  def CompilationUnit: Rule1[String] = rule(
    capture(
      optSemis ~ TopPackageSeq ~ optSemis ~ TopStatSeq ~ optSemis ~ WL
    )
  )
}
