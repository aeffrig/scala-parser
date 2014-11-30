package scalaParser
import language.implicitConversions
import syntax._
import org.parboiled2._

/**
 * Parser for Scala syntax.
 */
class ScalaSyntax(val input: ParserInput) extends Parser with Basic with Identifiers with Literals {
  /**
   * Parses all whitespace, excluding newlines. This is only
   * really useful in e.g. {} blocks, where we want to avoid
   * capturing newlines so semicolon-inference would work
   */
  def WS = rule( zeroOrMore(Basic.WhitespaceChar | Literals.Comment) )

  /**
   * Parses whitespace, including newlines.
   * This is the default for most things
   */
  def WL = rule( zeroOrMore(Basic.WhitespaceChar | Literals.Comment | Basic.Newline) )


  /**
   * By default, all strings and characters greedily
   * capture all whitespace immediately before the token.
   */
  implicit private[this] def wspStr(s: String): R0 = rule( WL ~ str(s) )
  implicit private[this] def wspChar(s: Char): R0  = rule( WL ~ ch(s) )

  /**
   * Most keywords don't just require the correct characters to match,
   * they have to ensure that subsequent characters *don't* match in
   * order for it to be a keyword. This enforces that rule for key-words
   * (W) and key-operators (O) which have different non-match criteria.
   */
  object K {
    def W(s: String) = rule{ WL ~ Key.W(s) }
    def O(s: String) = rule{ WL ~ Key.O(s) }
  }

  def `:`        = rule( K.O(":") )
  def `=>`       = rule( K.O("=>") | K.O("⇒") )
  def `=`        = rule( K.O("=") )
  def `_`        = rule( K.W("_") )
  def `case`     = rule( K.W("case") )
  def `class`    = rule( K.W("class") )
  def `def`      = rule( K.W("def") )
  def `extends`  = rule( K.W("extends") )
  def `implicit` = rule( K.W("implicit") )
  def `lazy`     = rule( K.W("lazy") )
  def `object`   = rule( K.W("object") )
  def `package`  = rule( K.W("package") )
  def `this`     = rule( K.W("this") )
  def `trait`    = rule( K.W("trait") )
  def `type`     = rule( K.W("type") )
  def `val`      = rule( K.W("val") )
  def `var`      = rule( K.W("var") )
  def `with`     = rule( K.W("with") )
  def `macro`    = rule( K.W("macro") )
  def `yield`    = rule( K.W("yield") )

  /**
   * helper printing function
   */
  def pr(s: String) = rule { run(println(s"LOGGING $cursor: $s")) }

  def Id         = rule( WL ~ Identifiers.Id )
  def IdOrUscore = rule( Id | `_` )
  def VarId      = rule( WL ~ Identifiers.VarId )
  def Literal    = rule( WL ~ Literals.Literal )
  def Semi       = rule( WS ~ Basic.Semi )
  def Semis      = rule( oneOrMore(Semi) )
  def Newline    = rule( WL ~ Basic.Newline )
  def QualId     = rule( WL ~ oneOrMore(Id).separatedBy('.') )
  def Ids        = rule( oneOrMore(Id) separatedBy ',' )

  def NotNewline: R0    = rule( &( WS ~ !Basic.Newline ) )
  def OneNewlineMax: R0 = rule( WS ~ optNL ~ zeroOrMore(commentWs) ~ NotNewline )

  private def commentWs = rule( optWS ~ Literals.Comment ~ optWS ~ Basic.Newline )
  private def optWS     = rule( zeroOrMore(Basic.WhitespaceChar) )
  private def optNL     = rule( optional(Basic.Newline) )

  def StableId: R0 = {
    def ClassQualifier = rule { '[' ~ Id ~ ']' }
    rule {
      zeroOrMore(Id ~ '.') ~ (`this` | K.W("super") ~ optional(ClassQualifier)) ~ zeroOrMore('.' ~ Id) |
      Id ~ zeroOrMore('.' ~ Id)
    }
  }
  def ExistentialDcl    = rule( `type` ~ TypeDcl | `val` ~ ValDcl )
  def ExistentialDcls   = rule( oneOrMore(ExistentialDcl) separatedBy Semi )
  def ExistentialClause = rule( "forSome" ~ '{' ~ ExistentialDcls ~ '}' )

  private def InfixTypeRest = rule( ArrowType | optional(ExistentialClause) )
  private def TypeStart     = rule(
      Uscore
    | FunctionArgTypes ~ ArrowType
    | InfixType ~ InfixTypeRest
  )
  def Type: R0  = rule( TypeStart ~ TypeBounds )
  def InfixType = rule( CompoundType ~ zeroOrMore(NotNewline ~ Id ~ OneNewlineMax ~ CompoundType) )

  def CompoundType = {
    def RefineStat     = rule( `type` ~ TypeDef | Dcl | MATCH )
    def optRefineStats = rule( zeroOrMore(RefineStat) separatedBy Semis )
    def Refinement     = rule( OneNewlineMax ~ '{'  ~ optSemis ~ optRefineStats ~ optSemis ~ '}' )
    rule(
        AnnotType ~ WithClauses ~ optional(Refinement)
      | Refinement
    )
  }

  def AnnotType        = rule( SimpleType ~ optional(NotNewline ~ oneOrMore(NotNewline ~ Annotation)) )
  def SimpleType       = rule( ( ProductType | SingletonType | StableId ) ~ TypeSuffix )
  def TypeSuffix       = rule( zeroOrMore(TypeArgs | TypeProjection) )
  def TypeProjection   = rule( '#' ~ Id )
  def SingletonType    = rule( StableId ~ '.' ~ `type` )
  def ProductType      = rule( '(' ~ Types ~ ')' )
  def FunctionArgTypes = rule( '(' ~ optional(ParamTypes) ~ ')' )
  def TypeArgs         = rule( '[' ~ Types ~ ']' )
  def Types            = rule( oneOrMore(Type) separatedBy ',' )
  def TypePat          = rule( CompoundType )
  def WildcardStar     = rule( "_" ~ "*" )
  def Ascription       = rule( ":" ~ ( WildcardStar | Type | Annotations ) )
  def ParamType        = rule( ArrowType | RepeatedType | Type )
  def ArrowType        = rule( `=>` ~ Type )
  def RepeatedType     = rule( Type ~ "*" )
  def ParamTypes       = rule( oneOrMore(ParamType) separatedBy ',')

  private def Binding  = rule( IdOrUscore ~ optAscription )
  private def Bindings = rule( '(' ~ (zeroOrMore(Binding) separatedBy ',') ~ ')' )
  private def LArrow   = rule( K.O("<-") | K.O("←") )
  private def RArrow   = rule( K.O("=>") | K.O("⇒") )
  private def Uscore   = rule( K.W("_") )

  def ColonType      = rule( `:` ~ Type )
  def ColonParamType = rule( `:` ~ ParamType )
  def ColonInfixType = rule( `:` ~ InfixType )
  def ColonTypePat   = rule( `:` ~ TypePat )

  private def optInfixAscription = rule( optional(ColonInfixType) )
  private def optAscription      = rule( optional(ColonType) )
  def optParamType               = rule( optional(ColonParamType) )
  def optTypeArgs                = rule( optional(TypeArgs) )

  def LambdaHead = rule(
    (   Bindings
      | optImplicit ~ Id ~ optInfixAscription
      | `_` ~ optional(Ascription)
    ) ~ `=>`
  )

  def Enumerators(G: Boolean = false): R0 = {
    def Generator: R0 = rule( Pattern1 ~ LArrow ~ Expr0(G) ~ optional(Guard(G)) )
    def Enumerator: R0 = rule(
        Generator
      | Guard(G)
      | Pattern1 ~ `=` ~ Expr0(G)
    )

    rule( Generator ~ zeroOrMore(Semis ~ Enumerator) ~ WL )
  }
  def Expr = Expr0()
  def ExprSensitive = Expr0(true)
  def Expr0(G: Boolean = false): R0 = {
    def ElsePart        = rule( optSemi ~ K.W("else") ~ Expr0(G) )
    def TryPart         = rule( K.W("try") ~ Expr0(G) )
    def CatchPart       = rule( K.W("catch") ~ Expr0(G) )
    def FinPart         = rule( K.W("finally") ~ Expr0(G) )
    def MatchPart       = rule( "match" ~ '{' ~ CaseClauses ~ '}' )
    def IfCFlow         = rule( "if" ~ '(' ~ Expr ~ ')' ~ Expr0(G) ~ optional(ElsePart) )
    def WhileCFlow      = rule( "while" ~ '(' ~ Expr ~ ')' ~ Expr0(G) )
    def TryCFlow        = rule( TryPart ~ optional(CatchPart) ~ optional(FinPart) )
    def DoWhileCFlow    = rule( K.W("do") ~ Expr0(G) ~ optSemi ~ "while" ~ '(' ~ Expr ~ ')' )
    def EnumeratorsPart = rule( '(' ~ Enumerators() ~ ')' | '{' ~ Enumerators(true) ~ '}' )
    def ForCFlow        = rule( "for" ~ EnumeratorsPart ~ optYield ~ Expr0(G) )
    def AssignPart      = rule( SimpleExpr() ~ `=` ~ Expr0(G) )

    rule(
      zeroOrMore(LambdaHead) ~ (
          IfCFlow
        | WhileCFlow
        | TryCFlow
        | DoWhileCFlow
        | ForCFlow
        | K.W("throw") ~ Expr0(G)
        | K.W("return") ~ optional(Expr0(G))
        | AssignPart
        | PostfixExpr(G) ~ optional(MatchPart | Ascription)
      )
    )
  }

  def PostfixExpr(G: Boolean = false): R0 = {
    def PrefixOpchar = rule( WL ~ anyOf("-+~!") ~ WS ~ !Basic.OperatorChar )
    def PrefixExpr   = rule( optional(PrefixOpchar) ~ SimpleExpr(G) )
    def PostfixPart  = rule( NotNewline ~ Id ~ optional(Newline) )
    def Check        = if (G) OneNewlineMax else MATCH
    def Check0       = if (G) NotNewline else MATCH
    def InfixPart    = rule( Check0 ~ Id ~ optTypeArgs ~ Check ~ PrefixExpr )

    rule( PrefixExpr ~ zeroOrMore(InfixPart) ~ optional(PostfixPart) )
  }

  def SimpleExpr(G: Boolean = false): R0 = {
    def Path: R0 = rule {
      zeroOrMore(Id ~ '.') ~ `this` ~ zeroOrMore('.' ~ Id) |
      StableId
    }
    def Check0 = if (G) NotNewline else MATCH
    def SimpleExpr1 = rule{
      K.W("new") ~ (ClassTemplate | TemplateBody) |
      BlockExpr |
      Literal |
      Path |
      `_` |
      '(' ~ optional(Exprs) ~ ')'
    }
    rule {
      SimpleExpr1 ~
      zeroOrMore('.' ~ Id | TypeArgs | Check0 ~ ArgumentExprs) ~
      optional(Check0  ~ `_`)
    }
  }

  def Exprs: R0 = rule { oneOrMore(Expr).separatedBy(',') }
  def ArgumentExprs: R0 = rule {
    '(' ~ optional(Exprs ~ optional(`:` ~ `_` ~ '*')) ~ ')' |
      OneNewlineMax ~ BlockExpr
  }

  def BlockExpr: R0 = rule { '{' ~ (CaseClauses | Block) ~ optSemis ~  "}" }

  def BlockStats: R0 = {
    def Template: R0 = rule( optAnnotations ~ (optImplicit ~ optLazy ~ Def | zeroOrMore(LocalModifier) ~ TmplDef) )
    def BlockStat: R0 = rule(
        Import
      | Template
      | Expr0(true)
    )
    rule( oneOrMore(BlockStat) separatedBy Semis )
  }

  def Block: R0 = {
    def BlockEnd: R0   = rule( optSemis ~ &("}" | `case`) )
    def ResultExpr: R0 = rule( Expr0(true) | LambdaHead ~ Block )

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

  def CaseClause: R0  = rule( `case` ~ Pattern ~ optional(Guard()) ~ `=>` ~ Block )
  def CaseClauses: R0 = rule( oneOrMore(CaseClause) )

  def Guard(G: Boolean = false): R0 = rule { K.W("if") ~ PostfixExpr(G) }

  def Pattern: R0  = rule( oneOrMore(Pattern1) separatedBy '|' )
  def Pattern1: R0 = rule( `_` ~ ColonTypePat | VarId ~ ColonTypePat | Pattern2 )
  def Pattern2: R0 = {
    def Pattern3: R0 = rule( `_` ~ '*' | SimplePattern ~ zeroOrMore(Id ~ SimplePattern) )
    rule(
        VarId ~ "@" ~ Pattern3
      | Pattern3
      | VarId
    )
  }

  private def AnonTypedPattern = rule( `_` ~ optional(ColonTypePat) ~ !"*")
  private def TuplePattern     = rule( '(' ~ optional(ExtractorArgs) ~ ')' )
  private def ExtractorArgs    = rule( zeroOrMore(Pattern) separatedBy ',' )
  private def Extractor        = rule( StableId ~ optional('(' ~ ExtractorArgs ~ ')') )

  def SimplePattern: R0 = rule(
      AnonTypedPattern
    | Literal
    | TuplePattern
    | Extractor
    | VarId
  )

  def optTypeParamClause     = rule( optional(TypeParamClause) )
  def VariantTypeParam: R0   = rule( optAnnotations ~ optVariance ~ TypeParam )
  def VariantTypeParams      = rule( oneOrMore(VariantTypeParam) separatedBy ',' )
  def AnnotatedTypeParams    = rule( oneOrMore(optAnnotations ~ TypeParam) separatedBy ',' )
  def TypeParamClause: R0    = rule( '[' ~ VariantTypeParams ~ ']' )
  def FunTypeParamClause: R0 = rule( '[' ~ AnnotatedTypeParams ~ ']' )

  def ViewBound  = rule( K.O("<%") ~ Type )
  def LowBound   = rule( K.O(">:") ~ Type )
  def HighBound  = rule( K.O("<:") ~ Type )
  def TypeBounds = rule( optional(LowBound) ~ optional(HighBound) )

  def TypeParam: R0 = rule {
    IdOrUscore ~
    optTypeParamClause ~
    TypeBounds ~
    zeroOrMore(ViewBound) ~
    zeroOrMore(ColonType)
  }

  def ImplicitParamClause = rule( OneNewlineMax ~ '(' ~ `implicit` ~ Params ~ ')' )
  def ParamClauses: R0    = rule( zeroOrMore(ParamClause) ~ optional(ImplicitParamClause) )
  def ParamClause: R0     = rule( OneNewlineMax ~ '(' ~ optional(Params) ~ ')' )
  def Params: R0          = rule( zeroOrMore(Param) separatedBy ',' )
  def Param: R0           = rule( optAnnotations ~ Id ~ optParamType ~ optEqualsExpr )
  def optEqualsExpr       = rule( optional(`=` ~ Expr) )
  def VarianceAnnot       = rule( WL ~ anyOf("+-") )
  def optVariance         = rule( optional(VarianceAnnot) )

  private def DefOrDcl = rule( Def | Dcl )
  private def ValOrVar = rule( `val` | `var` )

  def ClassParam: R0 = rule( optAnnotations ~ optional(optModifiers ~ ValOrVar) ~ Id ~ ColonParamType ~ optEqualsExpr )
  def Modifier: R0   = rule( LocalModifier | AccessModifier | K.W("override") )
  def optModifiers   = rule( zeroOrMore(Modifier) )

  def LocalModifier: R0 = rule { K.W("abstract") | K.W("final") | K.W("sealed") | `implicit` | K.W("lazy") }
  def AccessModifier: R0 = {
    def AccessQualifier: R0 = rule { '[' ~ (`this` | Id) ~ ']' }
    rule { (K.W("private") | K.W("protected")) ~ optional(AccessQualifier) }
  }

  def Annotation: R0 = rule( '@' ~ !Identifiers.Operator ~ SimpleType ~  zeroOrMore(ArgumentExprs) )
  def Annotations    = rule( oneOrMore(Annotation) )
  def optAnnotations = rule( zeroOrMore(Annotation) )

  def TemplateBody: R0 = rule( '{' ~ optional(SelfType) ~ optSemis ~ TemplateStats ~ optSemis ~ '}' )
  def TemplateStat: R0 = rule(
      Import
    | zeroOrMore(Annotation ~ OneNewlineMax) ~ optModifiers ~ DefOrDcl
    | Expr0(true)
  )

  def TemplateStats = rule( zeroOrMore(TemplateStat) separatedBy Semis )
  def optSemi       = rule( optional(Semi) )
  def optSemis      = rule( optional(Semis) )
  def SelfType: R0  = rule(
      `this` ~ ColonInfixType ~ `=>`
    | IdOrUscore ~ optInfixAscription ~ `=>`
  )

  def Import: R0 = {
    def ImportExpr: R0      = rule( StableId ~ optional('.' ~ (`_` | ImportSelectors)) )
    def ImportSelectors: R0 = rule( '{' ~ zeroOrMore(ImportSelector ~ ',') ~ (ImportSelector | `_`) ~ '}' )
    def ImportSelector: R0  = rule( Id ~ optional(`=>` ~ IdOrUscore) )

    rule( K.W("import") ~ oneOrMore(ImportExpr).separatedBy(',') )
  }

  def Dcl: R0 = rule(
       `val` ~ ValDcl
    |  `var` ~ ValDcl
    |  `def` ~ FunSig ~ optAscription
    | `type` ~ TypeDcl
  )
  def FunSig: R0  = rule( Id ~ optional(FunTypeParamClause) ~ ParamClauses )
  def ValDcl: R0  = rule( Ids ~ ColonType )
  def TypeDcl: R0 = rule( Id ~ optTypeParamClause ~ TypeBounds )

  def PatVarDef: R0 = {
    def PatDef: R0 = rule { oneOrMore(Pattern2).separatedBy(',') ~ optAscription ~ `=` ~ Expr0(true) }
    def VarDef: R0 = rule { Ids ~ ColonType ~ `=` ~ `_` | PatDef }

    rule(
        `val` ~ PatDef
      | `var` ~ VarDef
    )
  }

  def Def: R0 = {
    def ConstrExpr: R0 = rule( ConstrBlock | SelfInvocation )
    def FunBody: R0 = rule(
        `=` ~ optMacro ~ Expr0(true)
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

  def TypeDef: R0 = rule( Id ~ optTypeParamClause ~ `=` ~ Type )

  private def WithClauses   = rule( zeroOrMore(`with` ~ AnnotType) )
  private def TraitParents  = rule( AnnotType ~ WithClauses )
  private def TraitTemplate = rule( optEarlyDefs ~ TraitParents ~ optional(TemplateBody) )

  def TmplDef: R0 = {
    def ClassParams           = rule( oneOrMore(ClassParam) separatedBy ',' )
    def ImplicitClause: R0    = rule( OneNewlineMax ~ '(' ~ `implicit` ~ ClassParams ~ ')' )
    def ClassParamClause: R0  = rule( OneNewlineMax ~ '(' ~ optional(ClassParams) ~ ')' )
    def ClassParamClauses: R0 = rule( oneOrMore(ClassParamClause) ~ optional(ImplicitClause) | ImplicitClause )
    def Annot: R0             = rule( '@' ~ SimpleType ~ ArgumentExprs )
    def ConstrPrelude: R0     = rule(
      NotNewline ~ (
          oneOrMore(Annot) ~ optional(AccessModifier)
        | zeroOrMore(Annot) ~ AccessModifier
      )
    )
    def ClassDef: R0 = rule {
      Id ~
      optTypeParamClause ~
      optional(ConstrPrelude) ~
      optional(ClassParamClauses) ~
      ClassTemplateOpt
    }
    def TraitTemplateOpt: R0 = rule( `extends` ~ TraitTemplate | optional(optExtends ~ TemplateBody) )
    def TraitDef: R0         = rule( Id ~ optTypeParamClause ~ TraitTemplateOpt )

    rule(
        `trait` ~ TraitDef
      | optCase ~ `class` ~ ClassDef
      | optCase ~ `object` ~ ObjectDef
    )
  }

  def optYield             = rule( optional(`yield`) )
  def optImplicit          = rule( optional(`implicit`) )
  def optMacro             = rule( optional(`macro`) )
  def optExtends           = rule( optional(`extends`) )
  def optLazy              = rule( optional(`lazy`) )
  def optCase              = rule( optional(`case`) )
  def ObjectDef: R0        = rule( Id ~ ClassTemplateOpt )
  def ClassTemplateOpt: R0 = rule(
      `extends` ~ ClassTemplate
    | optional(optExtends ~ TemplateBody)
  )

  def ClassTemplate: R0 = {
    def Constr: R0       = rule( AnnotType ~ zeroOrMore(NotNewline ~ ArgumentExprs) )
    def ClassParents: R0 = rule( Constr ~ WithClauses )
    rule( optEarlyDefs ~ ClassParents ~ optional(TemplateBody) )
  }

  def EarlyDef: R0       = rule( zeroOrMore(Annotation ~ OneNewlineMax) ~ optModifiers ~ PatVarDef )
  def EarlyDefs: R0      = rule( '{' ~ optional(oneOrMore(EarlyDef) separatedBy Semis) ~ optSemis ~ '}' ~ `with` )
  def optEarlyDefs       = rule( optional(EarlyDefs) )
  def ConstrBlock: R0    = rule( '{' ~ SelfInvocation ~ optional(Semis ~ BlockStats) ~ optSemis ~ '}' )
  def SelfInvocation: R0 = rule( `this` ~ oneOrMore(ArgumentExprs) )
  def TopStatSeq: R0     = rule( oneOrMore(TopStat) separatedBy Semis )
  def FlatPackageStat    = rule( `package` ~ QualId ~ !(WS ~ "{") )
  def TopPackageSeq: R0  = rule( oneOrMore(FlatPackageStat) separatedBy Semis )

  private def PackageObject: R0 = rule( `package` ~ `object` ~ ObjectDef )
  private def Packaging: R0     = rule( `package` ~ QualId ~ '{' ~ optSemis ~ optional(TopStatSeq) ~ optSemis ~ '}' )
  private def TopStat: R0       = rule(
      Packaging
    | PackageObject
    | Import
    | zeroOrMore(Annotation ~ OneNewlineMax) ~ optModifiers ~ TmplDef
  )

  def CompilationUnit: Rule1[String] = rule(
    capture(
      optSemis ~
      (TopPackageSeq ~ optional(Semis ~ TopStatSeq) | TopStatSeq | MATCH) ~
      optSemis ~
      WL
    )
  )
}
