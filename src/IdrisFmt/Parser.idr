module IdrisFmt.Parser

import Data.List as L

import public Parser.Source as PS
import public Parser.Rule.Source as PRS
import public Core.Core as CC
import public Core.FC as CFC
import public Core.Name as CN
import public Core.Name.Namespace as CNN
import public Idris.Syntax as IS
import public Idris.Parser as IP
import public TTImp.TTImp as TT

import IdrisFmt.AST as AST
import IdrisFmt.Comments as C

%hide Libraries.Text.Bounded.WithBounds.val

%default covering

||| Errors produced during parsing or AST translation.
public export
data ParseError
  = LexError String
  | ParseErr String
  | UnsupportedFeature String

export
Show ParseError where
  show (LexError s)          = "Lex error: " ++ s
  show (ParseErr s)          = "Parse error: " ++ s
  show (UnsupportedFeature s) = "Unsupported: " ++ s

||| Translate Idris2 Name to formatter Name.
translateName : CN.Name -> AST.Name
translateName (UN (Basic s)) = AST.UN s
translateName (UN (Field s)) = AST.UN s
translateName (UN Underscore) = AST.UN "_"
translateName (MN s i) = AST.MN s i
translateName (NS ns n) = translateName n
translateName (Nested i n) = translateName n
translateName (CaseBlock s i) = AST.MN s i
translateName (WithBlock s i) = AST.MN s i
translateName (Resolved i) = AST.MN "resolved" i
translateName (PV n i) = translateName n
translateName (DN s n) = AST.UN s

||| Translate Idris2 OpStr to formatter OpStr.
translateOpStr : IS.OpStr -> AST.OpStr AST.Name
translateOpStr (OpSymbols n) = AST.OpSymbols (show n)
translateOpStr (Backticked n) = AST.Backticked (translateName n)

||| Translate Idris2 RigCount to formatter RigCount.
translateRig : Algebra.RigCount -> AST.RigCount
translateRig c = elimSemi AST.Rig0 AST.Rig1 (const AST.RigW) c

mutual
  ||| Translate compiler PStr to formatter StringPart.
  translatePStr : IS.PStr -> AST.StringPart AST.Name
  translatePStr (StrLiteral _ s) = AST.StrLit s
  translatePStr (StrInterp _ tm) = AST.StrInterp (translatePTerm tm)

  ||| Extract named Pi parameters from a telescope term.
  extractPiParams : IS.PTerm -> (List (AST.Name, AST.Expr AST.Name), IS.PTerm)
  extractPiParams (PPi _ _ _ (Just n) arg ret) =
    let (ps, ty) = extractPiParams ret
     in ((translateName n, translatePTerm arg) :: ps, ty)
  extractPiParams t = ([], t)

  ||| Translate data type telescope to params and return type.
  translateDataType : Maybe IS.PTerm -> (List (AST.Name, AST.Expr AST.Name), AST.Expr AST.Name)
  translateDataType Nothing = ([], AST.EType)
  translateDataType (Just t) = let (ps, t') = extractPiParams t in (ps, translatePTerm t')

  ||| Convert List1 to List.
  list1ToList : List1 a -> List a
  list1ToList (x ::: xs) = x :: xs

  ||| Translate BasicMultiBinder to list of (name, type) pairs.
  translateBasicMultiBinder : IS.BasicMultiBinder' CN.Name -> List (AST.Name, AST.Expr AST.Name)
  translateBasicMultiBinder (MkBasicMultiBinder rig names ty) =
    map (\n => (translateName (val n), translatePTerm ty)) (list1ToList names)

  ||| Translate PiInfo from compiler to formatter.
  translatePiInfo : Core.TT.Binder.PiInfo IS.PTerm -> AST.PiInfo (AST.Expr AST.Name)
  translatePiInfo Explicit = AST.Explicit
  translatePiInfo Implicit = AST.Implicit
  translatePiInfo AutoImplicit = AST.AutoImplicit
  translatePiInfo (DefImplicit t) = AST.DefImplicit (translatePTerm t)

  ||| Translate compiler PrimType to formatter string.
  translatePrimType : Core.TT.Primitive.PrimType -> String
  translatePrimType IntType = "Int"
  translatePrimType Int8Type = "Int8"
  translatePrimType Int16Type = "Int16"
  translatePrimType Int32Type = "Int32"
  translatePrimType Int64Type = "Int64"
  translatePrimType IntegerType = "Integer"
  translatePrimType Bits8Type = "Bits8"
  translatePrimType Bits16Type = "Bits16"
  translatePrimType Bits32Type = "Bits32"
  translatePrimType Bits64Type = "Bits64"
  translatePrimType StringType = "String"
  translatePrimType CharType = "Char"
  translatePrimType DoubleType = "Double"
  translatePrimType WorldType = "World"

  ||| Translate compiler Constant to formatter Expr (for primitive types).
  translateConstant : Core.TT.Primitive.Constant -> AST.Expr AST.Name
  translateConstant (I i) = AST.EPrim (AST.CInt (cast i))
  translateConstant (BI i) = AST.EPrim (AST.CInt i)
  translateConstant (Str s) = AST.EPrim (AST.CString s)
  translateConstant (Ch c) = AST.EPrim (AST.CChar c)
  translateConstant (Db d) = AST.EPrim (AST.CDouble d)
  translateConstant (PrT pt) = AST.ERef (AST.UN (translatePrimType pt))
  translateConstant _ = AST.EPrim (AST.CInt 0)

  ||| Translate compiler PTerm to formatter AST Expr.
  translatePTerm : IS.PTerm -> AST.Expr AST.Name
  translatePTerm (PRef _ n) = AST.ERef (translateName n)
  translatePTerm (PPi _ rig pii mn arg ret) =
    AST.EPi (translateRig rig) (translatePiInfo pii) (map translateName mn)
            (translatePTerm arg) (translatePTerm ret)
  translatePTerm (PLam _ rig pii pat ty scope) =
    AST.ELam (translateRig rig) (translatePiInfo pii)
             (translatePTerm pat) (translatePTerm ty) (translatePTerm scope)
  translatePTerm (PApp _ f x) =
    AST.EApp (translatePTerm f) (translatePTerm x)
  translatePTerm (PPrimVal _ c) = translateConstant c
  translatePTerm (PType _) = AST.EType
  translatePTerm (PImplicit _) = AST.EImplicit
  translatePTerm (PInfer _) = AST.EImplicit
  translatePTerm (PHole _ _ s) = AST.EHole s
  translatePTerm (PDelayed _ lr x) = AST.EDelayed (translatePTerm x)
  translatePTerm (PDelay _ x) = AST.EDelay (translatePTerm x)
  translatePTerm (PForce _ x) = AST.EForce (translatePTerm x)
  translatePTerm (PBracketed _ x) = AST.EBracketed (translatePTerm x)
  translatePTerm (PDotted _ x) = AST.EDotted (translatePTerm x)
  translatePTerm (PAs _ _ n pat) = AST.EAs (translateName n) (translatePTerm pat)
  translatePTerm (POp _ lhsInfo op rhs) =
    AST.EOp (translatePTerm lhsInfo.val.getLhs) (translateOpStr op.val) (translatePTerm rhs)
  translatePTerm (PString _ _ strs) =
    AST.EString (map translatePStr strs)
  translatePTerm (PList _ _ xs) =
    AST.EList (map (translatePTerm . snd) xs)
  translatePTerm (PPair _ x y) =
    AST.EPair (translatePTerm x) (translatePTerm y)
  translatePTerm (PUnit _) =
    AST.EImplicit
  translatePTerm (PIfThenElse _ c t f) =
    AST.EIf (translatePTerm c) (translatePTerm t) (translatePTerm f)
  translatePTerm (PIdiom _ ns x) =
    AST.EIdiom (map show ns) (translatePTerm x)
  translatePTerm tm = AST.EHole ("unsupported_" ++ show tm)

||| Extract function name from a clause LHS.
getFnName : IS.PTerm -> Maybe AST.Name
getFnName (PRef _ n) = Just (translateName n)
getFnName (PApp _ f _) = getFnName f
getFnName (PNamedApp _ f _ _) = getFnName f
getFnName (PAutoApp _ f _) = getFnName f
getFnName (PBracketed _ t) = getFnName t
getFnName _ = Nothing

||| Translate compiler PClause to formatter AST Clause.
translatePClause : IS.PClause -> AST.Clause AST.Name
translatePClause (MkPatClause _ lhs rhs _) =
  AST.MkClause (translatePTerm lhs) (translatePTerm rhs)
translatePClause (MkWithClause _ lhs wps _ _) =
  AST.MkClause (translatePTerm lhs) (AST.EComment (MkComment LineComment "with clause" 0 0) (AST.EImplicit))
translatePClause (MkImpossible _ lhs) =
  AST.MkImposs (translatePTerm lhs)

||| Translate compiler PTypeDecl to formatter ConDecl.
translatePTypeDecl : IS.PTypeDecl -> AST.ConDecl AST.Name
translatePTypeDecl pty =
  let td = val pty
      ns = map (val . snd) (forget td.names)
   in case ns of
        [] => MkConDecl (AST.UN "unnamed") (translatePTerm td.type)
        (n :: _) => MkConDecl (translateName n) (translatePTerm td.type)

||| Translate compiler PField to formatter FieldDecl.
translatePField : IS.PField -> List (AST.FieldDecl AST.Name)
translatePField pf =
  let ns = WithData.get "names" pf
      f = val pf
   in map (\n => AST.MkFieldDecl (translateName (val n)) (translatePTerm (boundType f))) ns

||| Translate compiler PDecl to formatter AST Decl.
translatePDecl : IS.PDecl -> AST.Decl AST.Name
translatePDecl pdecl =
  let d = val pdecl
   in case d of
        IS.PClaim claim =>
          let tyDecl = val claim.type
              names = map (val . snd) (forget tyDecl.names)
           in case names of
                [] => AST.DComment (MkComment LineComment "empty claim" 0 0)
                (n :: _) => AST.DClaim [] (translateName n) (translatePTerm tyDecl.type)
                          (map translateFnOpt claim.opts)
        IS.PDef clauses =>
          case clauses of
            [] => AST.DComment (MkComment LineComment "empty PDef" 0 0)
            (c :: _) =>
              let lhs = case c of
                          MkPatClause _ l _ _ => l
                          MkWithClause _ l _ _ _ => l
                          MkImpossible _ l => l
               in case getFnName lhs of
                    Nothing => AST.DComment (MkComment LineComment "could not extract function name" 0 0)
                    Just n => AST.DDef [] n (map translatePClause clauses)
        IS.PData doc vis treq (MkPData _ tyname tycon opts datacons) =>
          let (params, ty) = translateDataType tycon
           in AST.DData [] (MkDataDecl (translateName tyname) params ty (map translatePTypeDecl datacons))
        IS.PData doc vis treq (MkPLater _ tyname tycon) =>
          AST.DComment (MkComment LineComment "forward data declaration" 0 0)
        IS.PParameters params decls =>
          AST.DComment (MkComment LineComment "PParameters not yet translated" 0 0)
        IS.PUsing usings decls =>
          AST.DComment (MkComment LineComment "PUsing not yet translated" 0 0)
        IS.PInterface vis constraints name doc params det conName methods =>
          let ps = concatMap translateBasicMultiBinder params
              parentTerms = map snd constraints
           in AST.DInterface [] (MkInterfaceDecl (translateName name) ps (map translatePTerm parentTerms) (map translatePDecl methods))
        IS.PImplementation vis opts pass implicits constraints name params implName nusing body =>
          AST.DImpl [] (MkImplDecl (map translateName implName) (translateName name) (map translatePTerm params) (map (map translatePDecl) body))
        IS.PRecord doc vis treq (MkPRecord tyname params opts conName decls) =>
          let ps = concatMap (translateBasicMultiBinder . bind) params
              fields = concatMap translatePField decls
           in AST.DRecord [] (MkRecordDecl (translateName tyname) ps Nothing fields)
        IS.PRecord doc vis treq (MkPRecordLater tyname params) =>
          AST.DComment (MkComment LineComment "forward record declaration" 0 0)
        IS.PFail msg decls =>
          AST.DComment (MkComment LineComment "PFail not yet translated" 0 0)
        IS.PMutual decls =>
          AST.DComment (MkComment LineComment "PMutual not yet translated" 0 0)
        IS.PFixity fixData =>
          let ops = map (\op => translateName op.toName) (forget fixData.operators)
           in AST.DFixity (MkFixityDecl (translateFixity fixData.fixity) fixData.precedence ops)
        IS.PNamespace ns decls =>
          AST.DNamespace [show ns] (map translatePDecl decls)
        IS.PTransform name lhs rhs =>
          AST.DComment (MkComment LineComment "PTransform not yet translated" 0 0)
        IS.PRunElabDecl tm =>
          AST.DComment (MkComment LineComment "PRunElabDecl not yet translated" 0 0)
        IS.PDirective dir =>
          AST.DComment (MkComment LineComment "PDirective not yet translated" 0 0)
        IS.PBuiltin bt n =>
          AST.DComment (MkComment LineComment "PBuiltin not yet translated" 0 0)
  where
    translateFnOpt : IS.PFnOpt -> AST.FnOpt
    translateFnOpt (IFnOpt TT.Inline) = AST.Inline
    translateFnOpt (IFnOpt TT.TCInline) = AST.TCInline
    translateFnOpt (IFnOpt TT.NoInline) = AST.NoInline
    translateFnOpt _ = AST.NoInline

    translateFixity : Core.TT.Fixity -> AST.Fixity
    translateFixity InfixL = AST.InfixL
    translateFixity InfixR = AST.InfixR
    translateFixity Infix  = AST.Infix
    translateFixity Prefix = AST.Prefix

||| Convert compiler Error to formatter ParseError.
fromError : CC.Error -> ParseError
fromError err = ParseErr (show err)

||| Split a string on a character.
splitString : Char -> String -> List String
splitString c s = go [] (unpack s)
  where
    go : List Char -> List Char -> List String
    go acc [] = [pack (reverse acc)]
    go acc (x :: xs) =
      if x == c then pack (reverse acc) :: go [] xs
      else go (x :: acc) xs

||| Translate compiler Import to formatter ImportDecl.
translateImport : IS.Import -> AST.ImportDecl
translateImport imp =
  let path = splitString '/' (toPath imp.path)
      alias = if show imp.nameAs == show imp.path then Nothing else Just (show imp.nameAs)
   in MkImportDecl imp.reexport path alias Nothing Nothing

||| Parse a full module from source text.
||| Uses Idris2's built-in parser.
export
parseModule : String -> Either ParseError (List (AST.Decl AST.Name))
parseModule src =
  let origin = CFC.Virtual CFC.Interactive
      result = PS.runParser origin Nothing src (IP.prog origin)
   in case result of
        Left err => Left (fromError err)
        Right (_, (_, mod)) =>
          let modName = show (IS.Module.moduleNS mod)
              header = AST.DModule modName []
              imps = map (AST.DImport . translateImport) (IS.Module.imports mod)
              decls = map translatePDecl (IS.Module.decls mod)
           in Right (header :: imps ++ decls)

||| Parse a single expression from source text.
export
parseExpr : String -> Either ParseError (AST.Expr AST.Name)
parseExpr src = ?rhs_parseExpr
