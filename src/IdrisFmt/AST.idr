module IdrisFmt.AST

import Data.List as L
import Data.SnocList as SL
import Data.String as S

import public IdrisFmt.Comments as C

%default total

||| Name representation, parameterized for future extension.
public export
data Name = UN String | MN String Int

export
Eq Name where
  (UN x)   == (UN y)   = x == y
  (MN x i) == (MN y j) = x == y && i == j
  _ == _ = False

export
Show Name where
  show (UN s)   = s
  show (MN s i) = s ++ "_" ++ show i

||| Multiplicity annotation (Quantitative Type Theory).
public export
data RigCount = Rig0 | Rig1 | RigW

||| Pi-type visibility/info.
public export
data PiInfo : Type -> Type where
  Explicit     : PiInfo ty
  Implicit     : PiInfo ty
  AutoImplicit : PiInfo ty
  DefImplicit  : (1 def : ty) -> PiInfo ty

||| Fixity direction.
public export
data Fixity = InfixL | InfixR | Infix | Prefix

||| Visibility modifier.
public export
data Visibility = Private | Export | Public

||| Function options.
public export
data FnOpt = Inline | TCInline | NoInline

||| Import declaration metadata.
public export
record ImportDecl where
  constructor MkImportDecl
  reexport : Bool
  name     : List String
  alias    : Maybe String
  hiding   : Maybe (List Name)
  exposing : Maybe (List Name)

||| Fixity declaration metadata.
public export
record FixityDecl where
  constructor MkFixityDecl
  fixity     : Fixity
  precedence : Nat
  names      : List Name

mutual
  ||| Top-level declaration AST.
  public export
  data Decl : Type -> Type where
    DModule      : String -> List String -> Decl nm
    DImport      : ImportDecl -> Decl nm
    DClaim       : List C.Comment -> Visibility -> nm -> Expr nm -> List FnOpt -> Decl nm
    DDef         : List C.Comment -> nm -> List (Clause nm) -> Decl nm
    DData        : List C.Comment -> Visibility -> DataDecl nm -> Decl nm
    DRecord      : List C.Comment -> Visibility -> RecordDecl nm -> Decl nm
    DInterface   : List C.Comment -> Visibility -> InterfaceDecl nm -> Decl nm
    DImpl        : List C.Comment -> Visibility -> ImplDecl nm -> Decl nm
    DFixity      : FixityDecl -> Decl nm
    DNamespace   : List String -> List (Decl nm) -> Decl nm
    DMutual      : List (Decl nm) -> Decl nm
    DParams      : List (nm, Maybe (Expr nm)) -> List (Decl nm) -> Decl nm
    DUsing       : List (Maybe nm, Expr nm) -> List (Decl nm) -> Decl nm
    DDirective   : String -> Decl nm
    DBuiltin     : String -> nm -> Decl nm
    DTransform   : String -> Expr nm -> Expr nm -> Decl nm
    DRunElab     : Expr nm -> Decl nm
    DComment     : C.Comment -> Decl nm
    DBlank       : Nat -> Decl nm

  ||| Expression AST.
  public export
  data Expr : Type -> Type where
    ERef        : nm -> Expr nm
    EPi         : RigCount -> PiInfo (Expr nm) -> Maybe nm -> Expr nm -> Expr nm -> Expr nm
    ELam        : RigCount -> PiInfo (Expr nm) -> Expr nm -> Expr nm -> Expr nm -> Expr nm
    ELet        : RigCount -> Expr nm -> Expr nm -> Expr nm -> Expr nm -> List (Clause nm) -> Expr nm
    EApp        : Expr nm -> Expr nm -> Expr nm
    EWithApp    : Expr nm -> Expr nm -> Expr nm
    ENamedApp   : Expr nm -> nm -> Expr nm -> Expr nm
    EAutoApp    : Expr nm -> Expr nm -> Expr nm
    EPostfixApp : Expr nm -> List nm -> Expr nm
    EPostfixAppPartial : List nm -> Expr nm
    EDelayed    : Expr nm -> Expr nm
    EDelay      : Expr nm -> Expr nm
    EForce      : Expr nm -> Expr nm
    ECase       : Expr nm -> List (Clause nm) -> Expr nm
    ELocal      : List (Decl nm) -> Expr nm -> Expr nm
    EList       : List (Expr nm) -> Expr nm
    ESnocList   : SnocList (Expr nm) -> Expr nm
    EPair       : Expr nm -> Expr nm -> Expr nm
    EString     : List (StringPart nm) -> Expr nm
    EDo         : Maybe String -> List (DoStmt nm) -> Expr nm
    EIdiom      : Maybe String -> Expr nm -> Expr nm
    EIf         : Expr nm -> Expr nm -> Expr nm -> Expr nm
    EHole       : String -> Expr nm
    EType       : Expr nm
    EUnit       : Expr nm
    EImplicit   : Expr nm
    EQuote      : Expr nm -> Expr nm
    EUnquote    : Expr nm -> Expr nm
    EPrim       : Constant -> Expr nm
    EOp         : Expr nm -> OpStr nm -> Expr nm -> Expr nm
    EPrefixOp   : OpStr nm -> Expr nm -> Expr nm
    ESectionL   : OpStr nm -> Expr nm -> Expr nm
    ESectionR   : Expr nm -> OpStr nm -> Expr nm
    EBracketed  : Expr nm -> Expr nm
    EAs         : nm -> Expr nm -> Expr nm
    EDotted     : Expr nm -> Expr nm
    EComment    : C.Comment -> Expr nm -> Expr nm

  ||| Pattern-matching clause.
  public export
  data Clause : Type -> Type where
    MkClause     : Expr nm -> Expr nm -> Clause nm
    MkCaseClause : Expr nm -> Expr nm -> Clause nm
    MkWith       : Expr nm -> List (Expr nm) -> List (Clause nm) -> Clause nm
    MkImposs     : Expr nm -> Clause nm

  ||| String interpolation part.
  public export
  data StringPart : Type -> Type where
    StrLit    : String -> StringPart nm
    StrInterp : Expr nm -> StringPart nm

  ||| Record field update.
  public export
  data FieldUpdate : Type -> Type where
    FSet      : List String -> Expr nm -> FieldUpdate nm
    FSetApp   : List String -> Expr nm -> FieldUpdate nm

  ||| Do-block statement.
  public export
  data DoStmt : Type -> Type where
    DoExp     : Expr nm -> DoStmt nm
    DoBind    : nm -> RigCount -> Maybe (Expr nm) -> Expr nm -> DoStmt nm
    DoBindPat : Expr nm -> Maybe (Expr nm) -> Expr nm -> List (Clause nm) -> DoStmt nm
    DoLet     : nm -> RigCount -> Expr nm -> DoStmt nm
    DoLetPat  : Expr nm -> Expr nm -> List (Clause nm) -> DoStmt nm
    DoRewrite : Expr nm -> DoStmt nm

  ||| Data type declaration.
  public export
  record DataDecl nm where
    constructor MkDataDecl
    name    : nm
    params  : List (nm, Expr nm)
    ty      : Expr nm
    cons    : List (ConDecl nm)

  ||| Constructor declaration.
  public export
  record ConDecl nm where
    constructor MkConDecl
    name : nm
    type : Expr nm

  ||| Record declaration.
  public export
  record RecordDecl nm where
    constructor MkRecordDecl
    name    : nm
    params  : List (nm, Expr nm)
    conName : Maybe nm
    fields  : List (FieldDecl nm)

  ||| Record field declaration.
  public export
  record FieldDecl nm where
    constructor MkFieldDecl
    name : nm
    type : Expr nm

  ||| Interface declaration.
  public export
  record InterfaceDecl nm where
    constructor MkInterfaceDecl
    name    : nm
    params  : List (nm, Expr nm)
    parents : List (Expr nm)
    methods : List (Decl nm)

  ||| Implementation declaration.
  public export
  record ImplDecl nm where
    constructor MkImplDecl
    name          : Maybe nm
    interfaceName : nm
    params        : List (Expr nm)
    body          : Maybe (List (Decl nm))

  ||| Operator string representation.
  public export
  data OpStr nm = OpSymbols String | Backticked nm

  ||| Primitive literal values.
  public export
  data Constant = CInt Integer | CString String | CChar Char | CDouble Double
