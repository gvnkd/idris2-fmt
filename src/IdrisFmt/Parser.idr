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

||| Translate compiler PClause to formatter AST Clause (for case alternatives, higher-order).
translatePClauseAsCase_ : (IS.PTerm -> AST.Expr AST.Name) -> IS.PClause -> AST.Clause AST.Name
translatePClauseAsCase_ trans (MkPatClause _ lhs rhs _) =
  AST.MkCaseClause (trans lhs) (trans rhs)
translatePClauseAsCase_ trans (MkWithClause _ lhs wps _ _) =
  AST.MkCaseClause (trans lhs) (AST.EComment (MkComment LineComment "with clause" 0 0) (AST.EImplicit))
translatePClauseAsCase_ trans (MkImpossible _ lhs) =
  AST.MkImposs (trans lhs)

||| Translate compiler PFieldUpdate to formatter Expr (higher-order to avoid mutual recursion).
translatePFieldUpdate_ : (IS.PTerm -> AST.Expr AST.Name) -> IS.PFieldUpdate' CN.Name -> AST.Expr AST.Name
translatePFieldUpdate_ trans (PSetField path v) =
  AST.EComment (MkComment LineComment ("set " ++ show path) 0 0) (trans v)
translatePFieldUpdate_ trans (PSetFieldApp path v) =
  AST.EComment (MkComment LineComment ("setApp " ++ show path) 0 0) (trans v)

||| Translate compiler PDo to formatter DoStmt (higher-order to avoid mutual recursion).
translatePDo_ : (IS.PTerm -> AST.Expr AST.Name) -> IS.PDo' CN.Name -> AST.DoStmt AST.Name
translatePDo_ trans (DoExp _ tm) = AST.DoExp (trans tm)
translatePDo_ trans (DoBind _ _ n rig ty tm) =
  AST.DoBind (translateName n) (translateRig rig) (map trans ty) (trans tm)
translatePDo_ trans (DoBindPat _ pat ty val alts) =
  AST.DoBindPat (trans pat) (map trans ty) (trans val) (map (translatePClauseAsCase_ trans) alts)
translatePDo_ trans (DoLet _ _ n rig ty val) =
  AST.DoLet (translateName n) (translateRig rig) (trans val)
translatePDo_ trans (DoLetPat _ pat ty val alts) =
  AST.DoLetPat (trans pat) (trans val) (map (translatePClauseAsCase_ trans) alts)
translatePDo_ trans (DoLetLocal _ decls) =
  AST.DoExp (AST.EComment (MkComment LineComment "local decls in do" 0 0) AST.EImplicit)
translatePDo_ trans (DoRewrite _ rule) =
  AST.DoRewrite (trans rule)

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

  ||| Translate PlainBinder to list of (name, type) pairs.
  translatePlainBinder : IS.PlainBinder' CN.Name -> List (AST.Name, Maybe (AST.Expr AST.Name))
  translatePlainBinder pb =
    let n : WithFC CN.Name = WithData.get "name" pb
        tm : IS.PTerm = val pb
     in [(translateName (val n), Just (translatePTerm tm))]

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

  ||| Translate compiler PClause to formatter AST Clause (for case alternatives).
  translatePClauseAsCase : IS.PClause -> AST.Clause AST.Name
  translatePClauseAsCase (MkPatClause _ lhs rhs _) =
    AST.MkCaseClause (translatePTerm lhs) (translatePTerm rhs)
  translatePClauseAsCase (MkWithClause _ lhs wps _ _) =
    AST.MkCaseClause (translatePTerm lhs) (AST.EComment (MkComment LineComment "with clause" 0 0) (AST.EImplicit))
  translatePClauseAsCase (MkImpossible _ lhs) =
    AST.MkImposs (translatePTerm lhs)

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
  translatePTerm (PCase _ _ scrut alts) =
    AST.ECase (translatePTerm scrut) (map (translatePClauseAsCase_ translatePTerm) alts)
  translatePTerm (PDoBlock _ ns stmts) =
    AST.EDo (map show ns) (map (translatePDo_ translatePTerm) stmts)
  translatePTerm (PUpdate _ updates) =
    AST.EList (map (translatePFieldUpdate_ translatePTerm) updates)
  translatePTerm (PRewrite _ rule tm) =
    AST.EComment (MkComment LineComment "rewrite" 0 0) (translatePTerm tm)
  translatePTerm (PComprehension _ tm stmts) =
    AST.EComment (MkComment LineComment "comprehension" 0 0) (translatePTerm tm)
  translatePTerm (PRange _ start step end) =
    AST.EComment (MkComment LineComment "range" 0 0) (translatePTerm start)
  translatePTerm (PRangeStream _ start step) =
    AST.EComment (MkComment LineComment "range stream" 0 0) (translatePTerm start)
  translatePTerm tm = AST.EHole ("unsupported_" ++ show tm)

||| Translate compiler PClause to formatter AST Clause.
translatePClause : IS.PClause -> AST.Clause AST.Name
translatePClause (MkPatClause _ lhs rhs _) =
  AST.MkClause (translatePTerm lhs) (translatePTerm rhs)
translatePClause (MkWithClause _ lhs wps _ _) =
  AST.MkClause (translatePTerm lhs) (AST.EComment (MkComment LineComment "with clause" 0 0) (AST.EImplicit))
translatePClause (MkImpossible _ lhs) =
  AST.MkImposs (translatePTerm lhs)

||| Translate compiler Directive to formatter string.
translateDirective : IS.Directive -> String
translateDirective (Hide (HideName n)) = "hide " ++ show n
translateDirective (Hide (HideFixity _ n)) = "hide " ++ show n
translateDirective (Unhide n) = "unhide " ++ show n
translateDirective (Logging Nothing) = "logging off"
translateDirective (Logging (Just _)) = "logging on"
translateDirective (LazyOn True) = "lazy on"
translateDirective (LazyOn False) = "lazy off"
translateDirective (UnboundImplicits True) = "unbound_implicits on"
translateDirective (UnboundImplicits False) = "unbound_implicits off"
translateDirective (AmbigDepth n) = "ambiguity_depth " ++ show n
translateDirective (TotalityDepth n) = "totality_depth " ++ show n
translateDirective (DefaultTotality _) = "default total"
translateDirective (PrefixRecordProjections True) = "prefix_record_projections on"
translateDirective (PrefixRecordProjections False) = "prefix_record_projections off"
translateDirective (AutoImplicitDepth n) = "auto_implicit_depth " ++ show n
translateDirective (NFMetavarThreshold n) = "nfmetavar_threshold " ++ show n
translateDirective (SearchTimeout n) = "search_timeout " ++ show n
translateDirective (CGAction cg act) = "cg " ++ cg ++ " " ++ act
translateDirective (Extension _) = "language"
translateDirective (Overloadable n) = "overloadable " ++ show n
translateDirective (Names n ns) = "names " ++ show n ++ " " ++ show ns
translateDirective (StartExpr tm) = "startExpr ..."
translateDirective (PairNames n1 n2 n3) = "pair " ++ show n1 ++ " " ++ show n2 ++ " " ++ show n3
translateDirective (RewriteName n1 n2) = "rewrite " ++ show n1 ++ " " ++ show n2
translateDirective (PrimInteger n) = "integerLit " ++ show n
translateDirective (PrimString n) = "stringLit " ++ show n
translateDirective (PrimChar n) = "charLit " ++ show n
translateDirective (PrimDouble n) = "doubleLit " ++ show n
translateDirective (PrimTTImp n) = "primTTImp " ++ show n
translateDirective (PrimName n) = "primName " ++ show n
translateDirective (PrimDecls n) = "primDecls " ++ show n
translateDirective (ForeignImpl n tms) = "foreign " ++ show n

||| Extract function name from a clause LHS.
getFnName : IS.PTerm -> Maybe AST.Name
getFnName (PRef _ n) = Just (translateName n)
getFnName (PApp _ f _) = getFnName f
getFnName (PNamedApp _ f _ _) = getFnName f
getFnName (PAutoApp _ f _) = getFnName f
getFnName (PBracketed _ t) = getFnName t
getFnName _ = Nothing

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
          let ps = case params of
                     Left pbs  => concatMap translatePlainBinder (forget pbs)
                     Right pbs => map (\(n, t) => (n, Just t)) (concatMap (translateBasicMultiBinder . bind) (forget pbs))
           in AST.DParams ps (map translatePDecl decls)
        IS.PUsing usings decls =>
          let us = map (\(mn, tm) => (map translateName mn, translatePTerm tm)) usings
           in AST.DUsing us (map translatePDecl decls)
        IS.PInterface vis constraints name doc params det conName methods =>
          let ps = concatMap translateBasicMultiBinder params
              parentTerms = map snd constraints
           in AST.DInterface [] (MkInterfaceDecl (translateName name) ps (map translatePTerm parentTerms) (map translatePDecl methods))
        IS.PImplementation vis opts pass implicits constraints name params implName nusing body =>
          AST.DImpl [] (MkImplDecl (map translateName implName) (translateName name) (map translatePTerm params) (map (map translatePDecl) body))
        IS.PRecord doc vis treq (MkPRecord tyname params opts conName decls) =>
          let ps = concatMap (translateBasicMultiBinder . bind) params
              fields = concatMap translatePField decls
              con = map (translateName . val) conName
           in AST.DRecord [] (MkRecordDecl (translateName tyname) ps con fields)
        IS.PRecord doc vis treq (MkPRecordLater tyname params) =>
          AST.DComment (MkComment LineComment "forward record declaration" 0 0)
        IS.PFail msg decls =>
          AST.DComment (MkComment LineComment "PFail not yet translated" 0 0)
        IS.PMutual decls =>
          AST.DMutual (map translatePDecl decls)
        IS.PFixity fixData =>
          let ops = map (\op => translateName op.toName) (forget fixData.operators)
           in AST.DFixity (MkFixityDecl (translateFixity fixData.fixity) fixData.precedence ops)
        IS.PNamespace ns decls =>
          AST.DNamespace [show ns] (map translatePDecl decls)
        IS.PTransform name lhs rhs =>
          AST.DTransform name (translatePTerm lhs) (translatePTerm rhs)
        IS.PRunElabDecl tm =>
          AST.DRunElab (translatePTerm tm)
        IS.PDirective dir =>
          AST.DDirective (translateDirective dir)
        IS.PBuiltin bt n =>
          AST.DBuiltin (show bt) (translateName n)
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
