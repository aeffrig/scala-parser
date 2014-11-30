package scalaParser
import language.implicitConversions
import syntax._
import org.parboiled2._

/**
 * Parser for Scala syntax.
 */
class ScalaSyntax(val input: ParserInput) extends Parser with Basic with Identifiers with Literals with Keywords {
  /**
   * Parses all whitespace, excluding newlines. This is only
   * really useful in e.g. {} blocks, where we want to avoid
   * capturing newlines so semicolon-inference would work
   */
  def WS = rule( (Basic.WhitespaceChar | Literals.Comment)* )

  /**
   * Parses whitespace, including newlines.
   * This is the default for most things
   */
  def WL = rule( (Basic.WhitespaceChar | Literals.Comment | Basic.Newline)* )

  /**
   * By default, all strings and characters greedily
   * capture all whitespace immediately before the token.
   */
  implicit private[this] def wspStr(s: String): R0 = rule( WL ~ str(s) )
  implicit private[this] def wspChar(s: Char): R0  = rule( WL ~ ch(s) )

  def LArrow       = rule( K.O("<-") | K.O("←") )
  def RArrow       = rule( `=>` | K.O("⇒") )
  def Uscore       = rule( `_` )
  def WildcardStar = rule( `_` ~ `*` )
  def SubType      = rule( K.O("<:") )
  def SuperType    = rule( K.O(">:") )

  /**
   * helper printing function
   */
  def pr(s: String) = rule { run(println(s"LOGGING $cursor: $s")) }

  def Id         = rule( WL ~ Identifiers.Id )
  def IdOrUscore = rule( Id | `_` )
  def VarId      = rule( WL ~ Identifiers.VarId )
  def Literal    = rule( WL ~ Literals.Literal )
  def Semi       = rule( WS ~ Basic.Semi )
  def Semis      = rule( Semi+ )
  def Newline    = rule( WL ~ Basic.Newline )
  def QualId     = rule( WL ~ (Id.+ sep '.') )
  def Ids        = rule( Id.+ sep ',' )

  private def commentWs = rule( optWS ~ Literals.Comment ~ optWS ~ Basic.Newline )
  private def optWS     = rule( Basic.WhitespaceChar* )
  private def optNL     = rule( Basic.Newline? )

  def ThisOrSuper = rule(
      `this`
    | `super` ~ ClassQualifier.?
  )

  def ClassQualifier = rule( '[' ~ Id ~ ']' )
  def StableId: R0 = rule(
      zeroOrMore(Id ~ '.') ~ ThisOrSuper ~ zeroOrMore('.' ~ Id)
    | Id ~ zeroOrMore('.' ~ Id)
  )

  private def InfixTypeRest = rule( ArrowType | ExistentialClause.? )
  private def TypeStart     = rule(
      Uscore
    | FunctionArgTypes ~ ArrowType
    | InfixType ~ InfixTypeRest
  )

  def CompoundType = {
    def RefineStat     = rule( `type` ~ TypeDef | Dcl | MATCH )
    def optRefineStats = rule( RefineStat.* sep Semis )
    def Refinement     = rule( OneNewlineMax ~ '{'  ~ optSemis ~ optRefineStats ~ optSemis ~ '}' )
    rule(
        AnnotType ~ WithClauses ~ Refinement.?
      | Refinement
    )
  }

  def AnnotType         = rule( SimpleType ~ optional(NotNewline ~ oneOrMore(NotNewline ~ Annotation)) )
  def ArrowType         = rule( `=>` ~ Type )
  def Ascription        = rule( `:` ~ ( WildcardStar | Type | Annotations ) )
  def ColonInfixType    = rule( `:` ~ InfixType )
  def ColonParamType    = rule( `:` ~ ParamType )
  def ColonType         = rule( `:` ~ Type )
  def ColonTypePat      = rule( `:` ~ TypePat )
  def ExistentialClause = rule( `forSome` ~ '{' ~ ExistentialDcls ~ '}' )
  def ExistentialDcl    = rule( `type` ~ TypeDcl | `val` ~ ValDcl )
  def ExistentialDcls   = rule( ExistentialDcl.+ sep Semi )
  def FunctionArgTypes  = rule( '(' ~ ParamTypes.? ~ ')' )
  def InfixType         = rule( CompoundType ~ zeroOrMore(NotNewline ~ Id ~ OneNewlineMax ~ CompoundType) )
  def NotNewline: R0    = rule( &( WS ~ !Basic.Newline ) )
  def OneNewlineMax: R0 = rule( WS ~ optNL ~ zeroOrMore(commentWs) ~ NotNewline )
  def ParamType         = rule( ArrowType | RepeatedType | Type )
  def ParamTypes        = rule( ParamType.+ sep ',')
  def ProductType       = rule( '(' ~ Types ~ ')' )
  def RepeatedType      = rule( Type ~ `*` )
  def SimpleType        = rule( ( ProductType | SingletonType | StableId ) ~ TypeSuffix )
  def SingletonType     = rule( StableId ~ '.' ~ `type` )
  def Type: R0          = rule( TypeStart ~ TypeBounds )
  def TypeArgs          = rule( '[' ~ Types ~ ']' )
  def TypePat           = rule( CompoundType )
  def TypeProjection    = rule( '#' ~ Id )
  def TypeSuffix        = rule( (TypeArgs | TypeProjection)* )
  def Types             = rule( Type.+ sep ',' )

  private def Binding            = rule( IdOrUscore ~ optAscription )
  private def Bindings           = rule( '(' ~ (Binding.* sep ',') ~ ')' )
  private def optInfixAscription = rule( ColonInfixType.? )
  private def optAscription      = rule( ColonType.? )
  def optParamType               = rule( ColonParamType.? )
  def optTypeArgs                = rule( TypeArgs.? )

  def EnumeratorsPart = rule(
      '(' ~ NotSensitive.Enumerators ~ ')'
    | '{' ~ IsSensitive.Enumerators ~ '}'
  )

  object IsSensitive extends SensitiveRules(semicolonInference = true)
  object NotSensitive extends SensitiveRules(semicolonInference = false)


  abstract class SensitiveRules(semicolonInference: Boolean) {
    def MaybeOneNewline: R0 = if (semicolonInference) OneNewlineMax else MATCH
    def MaybeNotNewline: R0 = if (semicolonInference) NotNewline else MATCH

    def AssignExpr = rule( NotSensitive.SimpleExpr ~ `=` ~ Expr )
    def DoExpr     = rule( `do` ~ Expr ~ optSemi ~ `while` ~ ParenExpr )
    def ForExpr    = rule( `for` ~ EnumeratorsPart ~ optYield ~ Expr )
    def IfExpr     = rule( `if` ~ ParenExpr ~ Expr ~ optional(optSemi ~ ElsePart) )
    def ReturnExpr = rule( `return` ~ Expr.? )
    def ThrowExpr  = rule( `throw` ~ Expr )
    def TryExpr    = rule( TryPart ~ CatchPart.? ~ FinPart.? )
    def TupleExpr  = rule( '(' ~ Exprs.? ~ ')' )
    def WhileExpr  = rule( `while` ~ ParenExpr ~ Expr )

    def TryPart   = rule( `try` ~ Expr )
    def CatchPart = rule( `catch` ~ Expr )
    def FinPart   = rule( `finally` ~ Expr )
    def ElsePart  = rule( `else` ~ Expr )
    def MatchPart = rule( `match` ~ CaseBlock )

    def Enumerators     = rule( Generator ~ (Semis ~ Enumerator).* ~ WL )
    def Generator: R0   = rule( Pattern1 ~ LArrow ~ Expr ~ Guard.? )
    def Guard: R0       = rule( `if` ~ PostfixExpr )
    def InfixPart       = rule( MaybeNotNewline ~ Id ~ optTypeArgs ~ MaybeOneNewline ~ PrefixExpr )
    def PostfixExpr: R0 = rule( PrefixExpr ~ InfixPart.* ~ PostfixPart.? )
    def PostfixPart     = rule( NotNewline ~ Id ~ Newline.? )
    def PrefixExpr      = rule( PrefixOpchar.? ~ SimpleExpr )
    def PrefixOpchar    = rule( WL ~ anyOf("-+~!") ~ WS ~ !Basic.OperatorChar )

    def SimpleExprStart = rule(
        NewExpr
      | BlockExpr
      | Literal
      | Path
      | Uscore
      | TupleExpr
    )
    def SimpleExprPart = rule(
        '.' ~ Id
      | TypeArgs
      | MaybeNotNewline ~ ArgumentExprs
    )
    def SimpleExpr: R0 = rule( SimpleExprStart ~ SimpleExprPart.* ~ (MaybeNotNewline ~ `_`).? )

    def Path: R0 = rule(
        zeroOrMore(Id ~ '.') ~ `this` ~ zeroOrMore('.' ~ Id)
      | StableId
    )
    def Enumerator: R0 = rule(
        Generator
      | Guard
      | Pattern1 ~ `=` ~ Expr
    )
    def Expr: R0 = rule(
      zeroOrMore(LambdaHead) ~ (
          IfExpr
        | WhileExpr
        | TryExpr
        | DoExpr
        | ForExpr
        | ThrowExpr
        | ReturnExpr
        | AssignExpr
        | PostfixExpr ~ optional(MatchPart | Ascription)
      )
    )
  }

  def LambdaHead = rule(
    (   Bindings
      | optImplicit ~ Id ~ optInfixAscription
      | `_` ~ Ascription.?
    ) ~ `=>`
  )

  def ArgumentExprs: R0 = rule(
      '(' ~ optional(Exprs ~ VarargsStar.?) ~ ')'
    | OneNewlineMax ~ BlockExpr
  )

  def BlockStats: R0 = {
    def Template: R0 = rule( optAnnotations ~ (optImplicit ~ optLazy ~ Def | LocalModifier.* ~ TmplDef) )
    def BlockStat: R0 = rule(
        Import
      | Template
      | ExprSensitive
    )
    rule( BlockStat.+ sep Semis )
  }

  def Block: R0 = {
    def BlockEnd: R0   = rule( optSemis ~ &("}" | `case`) )
    def ResultExpr: R0 = rule( ExprSensitive | LambdaHead ~ Block )

    rule {
      zeroOrMore(LambdaHead) ~
      optSemis ~
      (
        ResultExpr ~ BlockEnd |
        BlockStats ~ optional(Semis ~ ResultExpr) ~ BlockEnd |
        MATCH ~ BlockEnd
      )
    }
  }

  def Pattern: R0  = rule( Pattern1.+ sep '|' )
  def Pattern1: R0 = rule( `_` ~ ColonTypePat | VarId ~ ColonTypePat | Pattern2 )
  def Pattern2: R0 = {
    def Pattern3: R0 = rule( `_` ~ '*' | SimplePattern ~ zeroOrMore(Id ~ SimplePattern) )
    rule(
        VarId ~ "@" ~ Pattern3
      | Pattern3
      | VarId
    )
  }

  private def AnonTypedPattern = rule( `_` ~ ColonTypePat.? ~ !'*' )
  private def TuplePattern     = rule( '(' ~ ExtractorArgs.? ~ ')' )
  private def ExtractorArgs    = rule( Pattern.* sep ',' )
  private def Extractor        = rule( StableId ~ TuplePattern.? )

  def SimplePattern: R0 = rule(
      AnonTypedPattern
    | Literal
    | TuplePattern
    | Extractor
    | VarId
  )

  def AccessModifier      = rule( (`private` | `protected`) ~ optional(AccessQualifier) )
  def AccessQualifier     = rule( '[' ~ (`this` | Id) ~ ']' )
  def AllTypeBounds       = rule( TypeBounds ~ zeroOrMore(ViewBound) ~ zeroOrMore(ColonType) )
  def AnnotatedTypeParams = rule( oneOrMore(optAnnotations ~ TypeParam) sep ',' )
  def Annotation          = rule( '@' ~ !Identifiers.Operator ~ SimpleType ~  zeroOrMore(ArgumentExprs) )
  def Annotations         = rule( Annotation+ )
  def BlockExpr: R0       = rule( '{' ~ (CaseClauses | Block) ~ optSemis ~ '}' )
  def CaseBlock           = rule( '{' ~ CaseClauses ~ '}' )
  def CaseClause: R0      = rule( `case` ~ Pattern ~ optional(NotSensitive.Guard) ~ `=>` ~ Block )
  def CaseClauses: R0     = rule( oneOrMore(CaseClause) )
  def ClassParam          = rule( optAnnotations ~ optional(optModifiers ~ ValOrVar) ~ Id ~ ColonParamType ~ optEqualsExpr )
  def ConstrBlock: R0     = rule( '{' ~ SelfInvocation ~ optional(Semis ~ BlockStats) ~ optSemis ~ '}' )
  def EarlyDef: R0        = rule( zeroOrMore(Annotation ~ OneNewlineMax) ~ optModifiers ~ PatVarDef )
  def EarlyDefs: R0       = rule( '{' ~ optional(oneOrMore(EarlyDef) sep Semis) ~ optSemis ~ '}' ~ `with` )
  def Expr                = ExprNotSensitive
  def ExprNotSensitive    = NotSensitive.Expr
  def ExprSensitive       = IsSensitive.Expr
  def Exprs: R0           = rule( oneOrMore(Expr) sep ',' )
  def FunSig              = rule( Id ~ optional(FunTypeParamClause) ~ ParamClauses )
  def FunTypeParamClause  = rule( '[' ~ AnnotatedTypeParams ~ ']' )
  def HighBound           = rule( SubType ~ Type )
  def ImplicitParamClause = rule( OneNewlineMax ~ '(' ~ `implicit` ~ Params ~ ')' )
  def LocalModifier       = rule( `abstract` | `final` | `sealed` | `implicit` | `lazy` )
  def LowBound            = rule( SuperType ~ Type )
  def Modifier            = rule( LocalModifier | AccessModifier | `override` )
  def NewExpr             = rule( `new` ~ (ClassTemplate | TemplateBody) )
  def ObjectDef: R0       = rule( Id ~ ClassTemplateOpt )
  def Param               = rule( optAnnotations ~ Id ~ optParamType ~ optEqualsExpr )
  def ParamClause         = rule( OneNewlineMax ~ '(' ~ optional(Params) ~ ')' )
  def ParamClauses        = rule( zeroOrMore(ParamClause) ~ optional(ImplicitParamClause) )
  def Params              = rule( zeroOrMore(Param) sep ',' )
  def ParenExpr           = rule( '(' ~ ExprNotSensitive ~ ')' )
  def SelfInvocation: R0  = rule( `this` ~ oneOrMore(ArgumentExprs) )
  def TemplateStats       = rule( zeroOrMore(TemplateStat) sep Semis )
  def TypeBounds          = rule( optional(LowBound) ~ optional(HighBound) )
  def TypeDcl             = rule( Id ~ optTypeParamClause ~ TypeBounds )
  def TypeDef             = rule( Id ~ optTypeParamClause ~ `=` ~ Type )
  def TypeParam: R0       = rule( IdOrUscore ~ optTypeParamClause ~ AllTypeBounds )
  def TypeParamClause     = rule( '[' ~ VariantTypeParams ~ ']' )
  def ValDcl              = rule( Ids ~ ColonType )
  def VarargsStar         = rule( `:` ~ `_` ~ '*' )
  def VarianceAnnot       = rule( WL ~ anyOf("+-") )
  def VariantTypeParam    = rule( optAnnotations ~ optVariance ~ TypeParam )
  def VariantTypeParams   = rule( oneOrMore(VariantTypeParam) sep ',' )
  def ViewBound           = rule( K.O("<%") ~ Type )

  def optAnnotations      = rule( zeroOrMore(Annotation) )
  def optCase             = rule( optional(`case`) )
  def optEarlyDefs        = rule( optional(EarlyDefs) )
  def optEqualsExpr       = rule( optional(`=` ~ Expr) )
  def optExtends          = rule( optional(`extends`) )
  def optImplicit         = rule( optional(`implicit`) )
  def optLazy             = rule( optional(`lazy`) )
  def optMacro            = rule( optional(`macro`) )
  def optModifiers        = rule( zeroOrMore(Modifier) )
  def optSemi             = rule( optional(Semi) )
  def optSemis            = rule( optional(Semis) )
  def optTypeParamClause  = rule( optional(TypeParamClause) )
  def optVariance         = rule( optional(VarianceAnnot) )
  def optYield            = rule( optional(`yield`) )

  private def DefOrDcl          = rule( Def | Dcl )
  private def ValOrVar          = rule( `val` | `var` )
  private def WithClauses       = rule( zeroOrMore(`with` ~ AnnotType) )
  private def TraitParents      = rule( AnnotType ~ WithClauses )
  private def TraitTemplate     = rule( optEarlyDefs ~ TraitParents ~ optional(TemplateBody) )
  private def PackageObject: R0 = rule( `package` ~ `object` ~ ObjectDef )
  private def Packaging: R0     = rule( `package` ~ QualId ~ '{' ~ optSemis ~ optional(TopStatSeq) ~ optSemis ~ '}' )

  def TemplateBody: R0 = rule( '{' ~ optional(SelfType) ~ optSemis ~ TemplateStats ~ optSemis ~ '}' )
  def TemplateStat: R0 = rule(
      Import
    | zeroOrMore(Annotation ~ OneNewlineMax) ~ optModifiers ~ DefOrDcl
    | ExprSensitive
  )

  def SelfType: R0  = rule(
      `this` ~ ColonInfixType ~ `=>`
    | IdOrUscore ~ optInfixAscription ~ `=>`
  )

  def Import: R0 = {
    def ImportExpr: R0      = rule( StableId ~ optional('.' ~ (`_` | ImportSelectors)) )
    def ImportSelectors: R0 = rule( '{' ~ zeroOrMore(ImportSelector ~ ',') ~ (ImportSelector | `_`) ~ '}' )
    def ImportSelector: R0  = rule( Id ~ optional(`=>` ~ IdOrUscore) )

    rule( `import` ~ (oneOrMore(ImportExpr) sep ',') )
  }

  def Dcl: R0 = rule(
       `val` ~ ValDcl
    |  `var` ~ ValDcl
    |  `def` ~ FunSig ~ optAscription
    | `type` ~ TypeDcl
  )

  def PatVarDef: R0 = {
    def PatDef: R0 = rule { oneOrMore(Pattern2).sep(',') ~ optAscription ~ `=` ~ ExprSensitive }
    def VarDef: R0 = rule { Ids ~ ColonType ~ `=` ~ `_` | PatDef }

    rule(
        `val` ~ PatDef
      | `var` ~ VarDef
    )
  }

  def Def: R0 = {
    def ConstrExpr: R0 = rule( ConstrBlock | SelfInvocation )
    def FunBody: R0 = rule(
        `=` ~ optMacro ~ ExprSensitive
      | OneNewlineMax ~ '{' ~ Block ~ '}'
    )
    def FunDef: R0 = rule(
        `this` ~ ParamClause ~ ParamClauses ~ (`=` ~ ConstrExpr | OneNewlineMax ~ ConstrBlock)
      | FunSig ~ optAscription ~ FunBody
    )

    rule(
        `def` ~ FunDef
      | `type` ~ TypeDef
      | PatVarDef
      | TmplDef
    )
  }

  def TmplDef: R0 = {
    def ClassParams           = rule( oneOrMore(ClassParam) sep ',' )
    def ImplicitClause: R0    = rule( OneNewlineMax ~ '(' ~ `implicit` ~ ClassParams ~ ')' )
    def ClassParamClause: R0  = rule( OneNewlineMax ~ '(' ~ optional(ClassParams) ~ ')' )
    def ClassParamClauses: R0 = rule( oneOrMore(ClassParamClause) ~ optional(ImplicitClause) | ImplicitClause )
    def Annot: R0             = rule( '@' ~ SimpleType ~ ArgumentExprs )
    def ConstrPrelude: R0     = rule( Annot.+ ~ AccessModifier.? | Annot.* ~ AccessModifier )
    def ClassDef: R0 = rule {
      Id ~
      optTypeParamClause ~
      optional(NotNewline ~ ConstrPrelude) ~
      optional(ClassParamClauses) ~
      ClassTemplateOpt
    }
    def TraitDef: R0         = rule( Id ~ optTypeParamClause ~ TraitTemplateOpt )

    rule(
        `trait` ~ TraitDef
      | optCase ~ `class` ~ ClassDef
      | optCase ~ `object` ~ ObjectDef
    )
  }

  def TraitTemplateOpt: R0 = rule(
      `extends` ~ TraitTemplate
    | optional(optExtends ~ TemplateBody)
  )
  def ClassTemplateOpt: R0 = rule(
      `extends` ~ ClassTemplate
    | optional(optExtends ~ TemplateBody)
  )

  def ClassTemplate: R0 = {
    def Constr: R0       = rule( AnnotType ~ zeroOrMore(NotNewline ~ ArgumentExprs) )
    def ClassParents: R0 = rule( Constr ~ WithClauses )
    rule( optEarlyDefs ~ ClassParents ~ optional(TemplateBody) )
  }

  private def TopStat: R0 = rule(
      Packaging
    | PackageObject
    | Import
    | zeroOrMore(Annotation ~ OneNewlineMax) ~ optModifiers ~ TmplDef
  )

  def FlatPackageStat   = rule( `package` ~ QualId ~ !(WS ~ "{") )
  def TopPackageSeq: R0 = rule( zeroOrMore(FlatPackageStat) sep Semis )
  def TopStatSeq: R0    = rule( zeroOrMore(TopStat) sep Semis )

  def CompilationUnit: Rule1[String] = rule(
    capture(
      optSemis ~ TopPackageSeq ~ optSemis ~ TopStatSeq ~ optSemis ~ WL
    )
  )
}
