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

  def Colon        = rule( `:` )
  def LArrow       = rule( K.O("<-") | K.O("←") )
  def RArrow       = rule( `=>` | K.O("⇒") )
  def SubType      = rule( K.O("<:") )
  def SuperType    = rule( K.O(">:") )
  def Uscore       = rule( `_` )
  def WildcardStar = rule( `_` ~ `*` )

  /**
   * helper printing function
   */
  def pr(s: String) = rule { run(println(s"LOGGING $cursor: $s")) }

  def CommentWS  = rule( SpaceWS ~ Literals.Comment ~ SpaceWS ~ Basic.Newline )
  def DotId      = rule( '.' ~ Id )
  def Id         = rule( WL ~ Identifiers.Id )
  def IdDot      = rule( Id ~ '.' )
  def IdOrUscore = rule( Id | Uscore )
  def Ids        = rule( rep1sep(Id, ',') )
  def Literal    = rule( WL ~ Literals.Literal )
  def NL         = rule( WL ~ Basic.Newline )
  def OptNL      = rule( WS ~ opt(Basic.Newline) )
  def QualId     = rule( WL ~ rep1sep(Id, '.') )
  def Semi       = rule( WS ~ Basic.Semi )
  def Semis      = rule( rep1(Semi) )
  def SpaceWS    = rule( rep(Basic.WhitespaceChar) )
  def VarId      = rule( WL ~ Identifiers.VarId )

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
    // | Uscore
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
  def ArrowType         = rule( `=>` ~ Type )
  def Ascription        = rule( Colon ~ ( WildcardStar | Type | Annotations ) )
  def Binding           = rule( IdOrUscore ~ opt(ColonType) )
  def ColonInfixType    = rule( Colon ~ InfixType )
  def ColonParamType    = rule( Colon ~ ParamType )
  def ColonType         = rule( Colon ~ Type )
  def ColonTypePat      = rule( Colon ~ TypePat )
  def ExistentialClause = rule( `forSome` ~ '{' ~ ExistentialDcls ~ '}' )
  def ExistentialDcl    = rule( `type` ~ TypeDcl | `val` ~ ValDcl )
  def ExistentialDcls   = rule( rep1sep(ExistentialDcl, Semi) )
  def FunctionArgTypes  = rule( '(' ~ opt(ParamTypes) ~ ')' )
  def InfixType         = rule( CompoundType ~ rep(NotNL ~ Id ~ OneNLMax ~ CompoundType) )
  def NotNL: R0         = rule( &( WS ~ !Basic.Newline ) )
  def OneNLMax: R0      = rule( OptNL ~ rep(CommentWS) ~ NotNL )
  def ParamType         = rule( ArrowType | RepeatedType | Type )
  def ParamTypes        = rule( rep1sep(ParamType, ',') )
  def ProductType       = rule( '(' ~ Types ~ ')' )
  def RepeatedType      = rule( Type ~ `*` )
  def SimpleType        = rule( ( ProductType | SingletonType | StableId ) ~ TypeSuffix )
  def SingletonType     = rule( StableId ~ '.' ~ `type` )
  def Type: R0          = rule( TypeStart ~ TypeBounds )
  def TypeArgs          = rule( '[' ~ Types ~ ']' )
  def TypePat           = rule( CompoundType )
  def TypeProjection    = rule( '#' ~ Id )
  def TypeSuffix        = rule( rep(TypeArgs | TypeProjection) )
  def Types             = rule( rep1sep(Type, ',') )

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
      | Pattern1 ~ `=` ~ Expr ~ opt(Guard) // XXX
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
    (   '(' ~ repsep(Binding, ',') ~ ')'
      | opt(`implicit`) ~ Id ~ opt(ColonInfixType)
      | Uscore ~ opt(Ascription)
    ) ~ `=>`
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

  def Patterns: R0 = rule( rep1sep(Pattern, ',') )
  def Pattern: R0  = rule( rep1sep(Pattern1, '|') )
  def Pattern1: R0 = rule( Uscore ~ ColonTypePat | VarId ~ ColonTypePat | Pattern2 )
  def Pattern2: R0 = {
    def Pattern3: R0 = rule( WildcardStar | SimplePattern ~ rep(Id ~ SimplePattern) )
    rule(
        VarId ~ '@' ~ Pattern3
      | Pattern3
      | VarId
    )
  }

  private def AnonTypedPattern = rule( !WildcardStar ~ Uscore ~ opt(ColonTypePat) )
  private def TuplePattern     = rule( '(' ~ opt(ExtractorArgs) ~ ')' )
  private def ExtractorArgs    = rule( repsep(Pattern, ',') )
  private def Extractor        = rule( StableId ~ opt(TuplePattern) )

  def SimplePattern: R0 = rule(
      XmlPattern
    | AnonTypedPattern
    | Literal
    | TuplePattern
    | Extractor
    | VarId
  )

  def AccessModifier      = rule( (`private` | `protected`) ~ opt(AccessQualifier) )
  def AccessQualifier     = rule( '[' ~ (`this` | Id) ~ ']' )
  def AllTypeBounds       = rule( TypeBounds ~ rep(ViewBound) ~ rep(ColonType) )
  def AnnotatedTypeParams = rule( rep1sep(rep(Annotation) ~ TypeParam, ',') )
  def Annotation          = rule( '@' ~ !Identifiers.Operator ~ SimpleType ~ rep(ArgumentExprs) )
  def Annotations         = rule( rep1(Annotation) )
  def BlockExpr: R0       = rule( '{' ~ (CaseClauses | Block) ~ optSemis ~ '}' )
  def CaseBlock           = rule( '{' ~ CaseClauses ~ '}' )
  def CaseClause: R0      = rule( `case` ~ Pattern ~ opt(NotSensitive.Guard) ~ `=>` ~ Block )
  def CaseClauses: R0     = rule( rep1(CaseClause) )
  def ClassParam          = rule( rep(Annotation) ~ opt(rep(Modifier) ~ ValOrVar) ~ Id ~ ColonParamType ~ opt(`=` ~ Expr) )
  def ConstrBlock: R0     = rule( '{' ~ SelfInvocation ~ opt(Semis ~ BlockStats) ~ optSemis ~ '}' )
  def EarlyDef: R0        = rule( rep(Annotation ~ OneNLMax) ~ rep(Modifier) ~ (PatVarDef | `type` ~ TypeDef) )
  def EarlyDefs: R0       = rule( '{' ~ repsep(EarlyDef, Semis) ~ optSemis ~ '}' ~ `with` )
  def Expr                = rule( NotSensitive.Expr )
  def ExprSensitive       = rule( IsSensitive.Expr )
  def Exprs: R0           = rule( rep1sep(Expr, ',') )
  def FunSig              = rule( Id ~ opt(FunTypeParamClause) ~ ParamClauses )
  def FunTypeParamClause  = rule( '[' ~ AnnotatedTypeParams ~ ']' )
  def HighBound           = rule( SubType ~ Type )
  def ImplicitParamClause = rule( OneNLMax ~ '(' ~ `implicit` ~ Params ~ ')' )
  def Import              = rule( `import` ~ ImportExprs )
  def ImportExpr          = rule( StableId ~ opt(ImportSuffix) )
  def ImportExprs         = rule( rep1sep(ImportExpr, ',') )
  def ImportRename        = rule( `=>` ~ IdOrUscore )
  def ImportSelector      = rule( Id ~ opt(ImportRename) )
  def ImportSelectors     = rule( '{' ~ rep(ImportSelector ~ ',') ~ (ImportSelector | Uscore) ~ '}' )
  def ImportSuffix        = rule( '.' ~ (Uscore | ImportSelectors) )
  def LocalModifier       = rule( `abstract` | `final` | `sealed` | `implicit` | `lazy` )
  def LowBound            = rule( SuperType ~ Type )
  def Modifier            = rule( LocalModifier | AccessModifier | `override` )
  def Param               = rule( rep(Annotation) ~ Id ~ opt(ColonParamType) ~ opt(`=` ~ Expr) )
  def ParamClause         = rule( OneNLMax ~ '(' ~ opt(Params) ~ ')' )
  def ParamClauses        = rule( rep(ParamClause) ~ opt(ImplicitParamClause) )
  def Params              = rule( repsep(Param, ',') )
  def ParenExpr           = rule( '(' ~ Expr ~ ')' )
  def SelfInvocation: R0  = rule( `this` ~ rep1(ArgumentExprs) )
  def TemplateStats       = rule( optSemis ~ repsep(TemplateStat, Semis) ~ optSemis )
  def TypeBounds          = rule( opt(LowBound) ~ opt(HighBound) )
  def TypeDcl             = rule( Id ~ opt(TypeParamClause) ~ TypeBounds )
  def TypeDef             = rule( Id ~ opt(TypeParamClause) ~ `=` ~ Type )
  def TypeParam: R0       = rule( IdOrUscore ~ opt(TypeParamClause) ~ AllTypeBounds )
  def TypeParamClause     = rule( '[' ~ VariantTypeParams ~ ']' )
  def ValDcl              = rule( Ids ~ ColonType )
  def VarargsStar         = rule( Colon ~ WildcardStar )
  def VarianceAnnot       = rule( WL ~ anyOf("+-") )
  def VariantTypeParam    = rule( rep(Annotation) ~ opt(VarianceAnnot) ~ TypeParam )
  def VariantTypeParams   = rule( rep1sep(VariantTypeParam, ',') )
  def ViewBound           = rule( K.O("<%") ~ Type )

  def optSemi       = rule( opt(Semi) )
  def optSemis      = rule( opt(Semis) )

  private def DefOrDcl          = rule( Def | Dcl )
  private def ValOrVar          = rule( `val` | `var` )
  private def WithClauses       = rule( rep(`with` ~ AnnotType) )
  private def PackageObject: R0 = rule( `package` ~ `object` ~ ObjectDef )
  private def Packaging: R0     = rule( `package` ~ QualId ~ '{' ~ optSemis ~ opt(TopStatSeq) ~ optSemis ~ '}' )

  def TemplateStat: R0 = rule(
      Import
    | rep(Annotation ~ OneNLMax) ~ rep(Modifier) ~ DefOrDcl
    | ExprSensitive
  )

  def SelfType: R0  = rule(
      `this` ~ ColonInfixType ~ `=>`
    | IdOrUscore ~ opt(ColonInfixType) ~ `=>`
  )

  def Dcl: R0 = rule(
       `val` ~ ValDcl
    |  `var` ~ ValDcl
    |  `def` ~ FunSig ~ opt(ColonType)
    | `type` ~ TypeDcl
  )

  def PatVarDef: R0 = {
    def PatDef: R0 = rule( rep1sep(Pattern2, ',') ~ opt(ColonType) ~ `=` ~ ExprSensitive )
    def VarDef: R0 = rule( Ids ~ ColonType ~ `=` ~ Uscore | PatDef )

    rule(
        `val` ~ PatDef
      | `var` ~ VarDef
    )
  }

  def Def: R0 = {
    def ConstrExpr: R0 = rule( ConstrBlock | SelfInvocation )
    def FunBody: R0 = rule(
        `=` ~ opt(`macro`) ~ ExprSensitive
      | OneNLMax ~ '{' ~ Block ~ '}'
    )
    def FunDef: R0 = rule(
        `this` ~ ParamClause ~ ParamClauses ~ (`=` ~ ConstrExpr | OneNLMax ~ ConstrBlock)
      | FunSig ~ opt(ColonType) ~ FunBody
    )

    rule(
        `def` ~ FunDef
      | `type` ~ TypeDef
      | PatVarDef
      | TmplDef
    )
  }

  def TemplateDefIntro = rule(
      `trait`
    | `class`
    | `object`
    | `case` ~ `class`
    | `case` ~ `object`
    | `package` ~ `object`
  )

  def TmplDef: R0 = {
    def ClassParams           = rule( rep1sep(ClassParam, ',') )
    def ImplicitClause: R0    = rule( OneNLMax ~ '(' ~ `implicit` ~ ClassParams ~ ')' )
    def ClassParamClause: R0  = rule( OneNLMax ~ '(' ~ opt(ClassParams) ~ ')' )
    def ClassParamClauses: R0 = rule( rep1(ClassParamClause) ~ opt(ImplicitClause) | ImplicitClause )
    def Annot: R0             = rule( '@' ~ SimpleType ~ ArgumentExprs )
    def ConstrPrelude: R0     = rule( rep1(Annot) ~ opt(AccessModifier) | rep(Annot) ~ AccessModifier )
    def ClassDef: R0          = rule( Id ~ opt(TypeParamClause) ~ opt(NotNL ~ ConstrPrelude) ~ opt(ClassParamClauses) ~ ClassTemplateOpt )

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
    | rep(Annotation ~ OneNLMax) ~ rep(Modifier) ~ TmplDef
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
