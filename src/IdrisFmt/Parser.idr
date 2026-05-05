module IdrisFmt.Parser
import public Core.Core as CC
import public Core.FC as CFC
import public Core.Metadata as CM
import public Core.Name as CN
import public Core.Name.Namespace as CNN
import Data.List as L
import Data.List1
import Data.String as S
import public Idris.Parser as IP
import public Idris.Syntax as IS
import IdrisFmt.AST as AST
import IdrisFmt.Comments as C
import Libraries.Data.WithDefault
import public Parser.Rule.Source as PRS
import public Parser.Source as PS
import public TTImp.TTImp as TT

%hide Libraries.Text.Bounded.WithBounds.val

%default covering

||| Errors produced during parsing or AST translation.
public export
data ParseError : Type where
  LexError : String -> ParseError
  ParseErr : String -> ParseError
  UnsupportedFeature : String -> ParseError

export
implementation Show ParseError where
  show (LexError s) = "Lex error: " ++ s
  show (ParseErr s) = "Parse error: " ++ s
  show (UnsupportedFeature s) = "Unsupported: " ++ s

||| Translate Idris2 Name to formatter Name.
translateName : CN.Name -> AST.Name
translateName (UN (Basic s)) = AST.UN s
translateName (UN (Field s)) = AST.UN s
translateName (UN Underscore) = AST.UN "_"
translateName (MN s i) = AST.MN s i
translateName (NS ns n) =
  AST.NS (CNN.unsafeUnfoldNamespace ns) (translateName n)
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
translatePClauseAsCase_ : (IS.PTerm -> AST.Expr AST.Name)
                            -> IS.PClause -> AST.Clause AST.Name
translatePClauseAsCase_ trans (MkPatClause _ lhs rhs _) =
  AST.MkCaseClause (trans lhs) (trans rhs)
translatePClauseAsCase_ trans (MkWithClause _ lhs wps _ _) =
  AST.MkCaseClause (trans lhs)
    (AST.EComment (MkComment LineComment "with clause" 0 0) (AST.EImplicit))
translatePClauseAsCase_ trans (MkImpossible _ lhs) = AST.MkImposs (trans lhs)

||| Translate compiler PFieldUpdate to formatter Expr (higher-order to avoid mutual recursion).
translatePFieldUpdate_ : (IS.PTerm -> AST.Expr AST.Name)
                           -> IS.PFieldUpdate' CN.Name -> AST.Expr AST.Name
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
  AST.DoBindPat (trans pat) (map trans ty) (trans val)
    (map (translatePClauseAsCase_ trans) alts)
translatePDo_ trans (DoLet _ _ n rig ty val) =
  AST.DoLet (translateName n) (translateRig rig) (trans val)
translatePDo_ trans (DoLetPat _ pat ty val alts) =
  AST.DoLetPat (trans pat) (trans val)
    (map (translatePClauseAsCase_ trans) alts)
translatePDo_ trans (DoLetLocal _ decls) =
  AST.DoExp
    (AST.EComment (MkComment LineComment "local decls in do" 0 0) AST.EImplicit)
translatePDo_ trans (DoRewrite _ rule) = AST.DoRewrite (trans rule)

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
  translateDataType : Maybe IS.PTerm
                        -> (List (AST.Name, AST.Expr AST.Name), AST.Expr AST.Name)
  translateDataType Nothing = ([], AST.EType)
  translateDataType (Just t) =
    let (ps, t') = extractPiParams t in (ps, translatePTerm t')
  ||| Translate PlainBinder to list of (name, type) pairs.
  translatePlainBinder : IS.PlainBinder' CN.Name
                           -> List (AST.Name, Maybe (AST.Expr AST.Name))
  translatePlainBinder pb =
    let n : WithFC CN.Name = WithData.get "name" pb
      in let tm : IS.PTerm = val pb
           in [(translateName (val n), Just (translatePTerm tm))]
  ||| Translate BasicMultiBinder to list of (name, type) pairs.
  translateBasicMultiBinder : IS.BasicMultiBinder' CN.Name
                                -> List (AST.Name, AST.Expr AST.Name)
  translateBasicMultiBinder (MkBasicMultiBinder rig names ty) =
    map (\n => (translateName (val n), translatePTerm ty)) (forget names)
  ||| Translate PiInfo from compiler to formatter.
  translatePiInfo : Core.TT.Binder.PiInfo IS.PTerm
                      -> AST.PiInfo (AST.Expr AST.Name)
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
  translateConstant (I8 i) = AST.EPrim (AST.CInt (cast i))
  translateConstant (I16 i) = AST.EPrim (AST.CInt (cast i))
  translateConstant (I32 i) = AST.EPrim (AST.CInt (cast i))
  translateConstant (I64 i) = AST.EPrim (AST.CInt (cast i))
  translateConstant (BI i) = AST.EPrim (AST.CInt i)
  translateConstant (B8 b) = AST.EPrim (AST.CInt (cast b))
  translateConstant (B16 b) = AST.EPrim (AST.CInt (cast b))
  translateConstant (B32 b) = AST.EPrim (AST.CInt (cast b))
  translateConstant (B64 b) = AST.EPrim (AST.CInt (cast b))
  translateConstant (Str s) = AST.EPrim (AST.CString s)
  translateConstant (Ch c) = AST.EPrim (AST.CChar c)
  translateConstant (Db d) = AST.EPrim (AST.CDouble d)
  translateConstant (PrT pt) = AST.ERef (AST.UN (translatePrimType pt))
  translateConstant WorldVal = AST.EPrim AST.CWorldVal
  ||| Translate compiler PTerm to formatter AST Expr.
  translatePTerm : IS.PTerm -> AST.Expr AST.Name
  translatePTerm (PRef _ n) = AST.ERef (translateName n)
  translatePTerm (PPi _ rig pii mn arg ret) =
    AST.EPi (translateRig rig) (translatePiInfo pii) (map translateName mn)
      (translatePTerm arg)
      (translatePTerm ret)
  translatePTerm (PLam _ rig pii pat ty scope) =
    AST.ELam (translateRig rig) (translatePiInfo pii) (translatePTerm pat)
      (translatePTerm ty)
      (translatePTerm scope)
  translatePTerm (PApp _ f x) = AST.EApp (translatePTerm f) (translatePTerm x)
  translatePTerm (PWithApp _ f x) =
    AST.EWithApp (translatePTerm f) (translatePTerm x)
  translatePTerm (PNamedApp _ f n x) =
    AST.ENamedApp (translatePTerm f) (translateName n) (translatePTerm x)
  translatePTerm (PAutoApp _ f x) =
    AST.EAutoApp (translatePTerm f) (translatePTerm x)
  translatePTerm (PPostfixApp _ rec fields) =
    AST.EPostfixApp (translatePTerm rec) (map (translateName . snd) fields)
  translatePTerm (PPostfixAppPartial _ fields) =
    AST.EPostfixAppPartial (map (translateName . snd) fields)
  translatePTerm (PPrefixOp _ op x) =
    AST.EPrefixOp (translateOpStr op.val) (translatePTerm x)
  translatePTerm (PSectionL _ op x) =
    AST.ESectionL (translateOpStr op.val) (translatePTerm x)
  translatePTerm (PSectionR _ x op) =
    AST.ESectionR (translatePTerm x) (translateOpStr op.val)
  translatePTerm (PEq _ l r) =
    AST.EOp (translatePTerm l) (AST.OpSymbols "=") (translatePTerm r)
  translatePTerm (PPrimVal _ c) = translateConstant c
  translatePTerm (PType _) = AST.EType
  translatePTerm (PImplicit _) = AST.EImplicit
  translatePTerm (PInfer _) = AST.EImplicit
  translatePTerm (PHole _ _ s) = AST.EHole s
  translatePTerm (PSearch _ d) = AST.EHole ("?search_" ++ show d)
  translatePTerm (PQuote _ x) = AST.EQuote (translatePTerm x)
  translatePTerm (PUnquote _ x) = AST.EUnquote (translatePTerm x)
  translatePTerm (PRunElab _ x) =
    AST.EComment (MkComment LineComment "runElab" 0 0) (translatePTerm x)
  translatePTerm (PBang _ x) = translatePTerm x
  translatePTerm (PDelayed _ lr x) = AST.EDelayed (translatePTerm x)
  translatePTerm (PDelay _ x) = AST.EDelay (translatePTerm x)
  translatePTerm (PForce _ x) = AST.EForce (translatePTerm x)
  translatePTerm (PBracketed _ x) = AST.EBracketed (translatePTerm x)
  translatePTerm (PDotted _ x) = AST.EDotted (translatePTerm x)
  translatePTerm (PAs _ _ n pat) = AST.EAs (translateName n) (translatePTerm pat)
  translatePTerm (POp _ lhsInfo op rhs) =
    AST.EOp (translatePTerm lhsInfo.val.getLhs) (translateOpStr op.val)
      (translatePTerm rhs)
  translatePTerm (PString _ _ strs) = AST.EString (map translatePStr strs)
  translatePTerm (PList _ _ xs) = AST.EList (map (translatePTerm . snd) xs)
  translatePTerm (PPair _ x y) = AST.EPair (translatePTerm x) (translatePTerm y)
  translatePTerm (PUnit _) = AST.EUnit
  translatePTerm (PIfThenElse _ c t f) =
    AST.EIf (translatePTerm c) (translatePTerm t) (translatePTerm f)
  translatePTerm (PIdiom _ ns x) = AST.EIdiom (map show ns) (translatePTerm x)
  translatePTerm (PCase _ _ scrut alts) =
    AST.ECase (translatePTerm scrut)
      (map (translatePClauseAsCase_ translatePTerm) alts)
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
  translatePTerm (PLet _ rig pat ty val scope alts) =
    AST.ELet (translateRig rig) (translatePTerm pat) (translatePTerm ty)
      (translatePTerm val)
      (translatePTerm scope)
      (map (translatePClauseAsCase_ translatePTerm) alts)
  translatePTerm (PLocal _ decls scope) =
    AST.ELocal (map (snd . translatePDecl) decls) (translatePTerm scope)
  translatePTerm (NewPi x) =
    let binder = x.val.binder
      in let info = translatePiInfo binder.info
           in let rig = binder.bind.rig
                in let name = translateName (val (head binder.bind.names))
                     in let ty = translatePTerm binder.bind.type
                          in let scope = translatePTerm x.val.scope
                               in AST.EPi (translateRig rig) info (Just name) ty
                                    scope
  translatePTerm (Forall x) =
    let (names, scope) = x.val
      in let ns = map (translateName . val) (forget names)
           in let sc = translatePTerm scope in AST.EForall ns sc
  translatePTerm (PMultiline _ _ _ _) =
    AST.EComment (MkComment LineComment "multiline string" 0 0) AST.EImplicit
  translatePTerm (PUnifyLog _ _ x) = translatePTerm x
  translatePTerm (PWithUnambigNames _ _ x) = translatePTerm x
  translatePTerm tm = AST.EHole ("unsupported_" ++ show tm)
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
  translateDirective (DefaultTotality treq) = "default " ++ show treq
  translateDirective (PrefixRecordProjections True) =
    "prefix_record_projections on"
  translateDirective (PrefixRecordProjections False) =
    "prefix_record_projections off"
  translateDirective (AutoImplicitDepth n) = "auto_implicit_depth " ++ show n
  translateDirective (NFMetavarThreshold n) = "nfmetavar_threshold " ++ show n
  translateDirective (SearchTimeout n) = "search_timeout " ++ show n
  translateDirective (CGAction cg act) = "cg " ++ cg ++ " " ++ act
  translateDirective (Extension _) = "language"
  translateDirective (Overloadable n) = "overloadable " ++ show n
  translateDirective (Names n ns) = "names " ++ show n ++ " " ++ show ns
  translateDirective (StartExpr tm) = "startExpr ..."
  translateDirective (PairNames n1 n2 n3) =
    "pair " ++ show n1 ++ " " ++ show n2 ++ " " ++ show n3
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
  getFnName (POp _ _ op _) = Just (translateName op.val.toName)
  getFnName (PPrefixOp _ op _) = Just (translateName op.val.toName)
  getFnName _ = Nothing
  ||| Translate compiler PTypeDecl to formatter ConDecl.
  translatePTypeDecl : IS.PTypeDecl -> AST.ConDecl AST.Name
  translatePTypeDecl pty =
    let td = val pty
      in let ns = map (val . snd) (forget td.names)
           in case ns of
                [] => MkConDecl (AST.UN "unnamed") (translatePTerm td.type)
                (n :: _) => MkConDecl (translateName n) (translatePTerm td.type)
  ||| Translate compiler PField to formatter FieldDecl.
  translatePField : IS.PField -> List (AST.FieldDecl AST.Name)
  translatePField pf = let ns = WithData.get "names" pf
                         in let f = val pf
                              in map
                                   (\n =>
                                      AST.MkFieldDecl (translateName (val n))
                                        (translatePTerm (boundType f)))
                                   ns
  ||| Extract start line from an FC.
  fcLine : CFC.FC -> Nat
  fcLine (CFC.MkFC _ start _) = cast (fst start)
  fcLine (CFC.MkVirtualFC _ start _) = cast (fst start)
  fcLine CFC.EmptyFC = 0
  ||| Translate compiler Visibility to formatter Visibility.
  translateVisibility : Core.TT.Visibility -> AST.Visibility
  translateVisibility Core.TT.Private = AST.Private
  translateVisibility Core.TT.Export = AST.Export
  translateVisibility Core.TT.Public = AST.Public
  ||| Translate compiler PFnOpt to formatter FnOpt.
  translateFnOpt : IS.PFnOpt -> AST.FnOpt
  translateFnOpt (IFnOpt TT.Inline) = AST.Inline
  translateFnOpt (IFnOpt TT.TCInline) = AST.TCInline
  translateFnOpt (IFnOpt TT.NoInline) = AST.NoInline
  translateFnOpt _ = AST.NoInline
  ||| Translate compiler Fixity to formatter Fixity.
  translateFixity : Core.TT.Fixity -> AST.Fixity
  translateFixity InfixL = AST.InfixL
  translateFixity InfixR = AST.InfixR
  translateFixity Infix = AST.Infix
  translateFixity Prefix = AST.Prefix
  ||| Translate compiler PClause to formatter AST Clause.
  translatePClause : IS.PClause -> AST.Clause AST.Name
  translatePClause (MkPatClause _ lhs rhs ws) =
    AST.MkClause (translatePTerm lhs) (translatePTerm rhs)
      (map (snd . translatePDecl) ws)
  translatePClause (MkWithClause _ lhs wps _ _) =
    AST.MkClause (translatePTerm lhs)
      (AST.EComment (MkComment LineComment "with clause" 0 0) (AST.EImplicit))
      []
  translatePClause (MkImpossible _ lhs) = AST.MkImposs (translatePTerm lhs)
  ||| Translate compiler PDecl to formatter AST Decl.
  ||| Returns the declaration paired with its source line number.
  translatePDecl : IS.PDecl -> (Nat, AST.Decl AST.Name)
  translatePDecl pdecl =
    let line = fcLine pdecl.fc
      in let d = val pdecl
           in let decl = case d of
                           IS.PClaim claim =>
                             let tyDecl = val claim.type
                               in let names = map (val . snd) (forget tyDecl.names)
                                    in let docs = docToComments tyDecl.doc
                                         in let vis = translateVisibility claim.vis
                                              in case names of
                                                   [] =>
                                                     AST.DComment
                                                       (MkComment LineComment
                                                          "empty claim"
                                                          0
                                                          0)
                                                   (n :: _) =>
                                                     AST.DClaim docs vis
                                                       (translateName n)
                                                       (translatePTerm tyDecl.type)
                                                       (map translateFnOpt
                                                          claim.opts)
                           IS.PDef clauses =>
                             case clauses of
                               [] =>
                                 AST.DComment
                                   (MkComment LineComment "empty PDef" 0 0)
                               (c :: _) =>
                                 let lhs = case c of
                                             MkPatClause _ l _ _ => l
                                             MkWithClause _ l _ _ _ => l
                                             MkImpossible _ l => l
                                   in case getFnName lhs of
                                        Nothing =>
                                          AST.DComment
                                            (MkComment LineComment
                                               "could not extract function name"
                                               0
                                               0)
                                        Just n =>
                                          AST.DDef [] n
                                            (map translatePClause clauses)
                           IS.PData doc vis treq
                             (MkPData _ tyname tycon opts datacons) =>
                             let (params, ty) = translateDataType tycon
                               in let docs = docToComments doc
                                    in let visibility = translateVisibility
                                                          (collapseDefault vis)
                                         in AST.DData docs visibility
                                              (MkDataDecl (translateName tyname)
                                                 params
                                                 ty
                                                 (map translatePTypeDecl datacons))
                           IS.PData doc vis treq (MkPLater _ _ _) =>
                             AST.DComment
                               (MkComment LineComment "forward data declaration" 0 0)
                           IS.PParameters params decls =>
                             let ps = case params of
                                        Left pbs =>
                                          concatMap translatePlainBinder (forget pbs)
                                        Right pbs =>
                                          map (\(n, t) => (n, Just t))
                                            (concatMap
                                               (translateBasicMultiBinder . bind)
                                               (forget pbs))
                               in AST.DParams ps (map (snd . translatePDecl) decls)
                           IS.PUsing usings decls =>
                             let us = map
                                        (\(mn, tm) =>
                                           (map translateName mn, translatePTerm tm))
                                        usings
                               in AST.DUsing us (map (snd . translatePDecl) decls)
                           IS.PInterface vis constraints name doc params det conName
                             methods =>
                             let ps = concatMap translateBasicMultiBinder params
                               in let parentTerms = map snd constraints
                                    in let docs = docToComments doc
                                         in let visibility = translateVisibility
                                                               (collapseDefault vis)
                                              in AST.DInterface docs visibility
                                                   (MkInterfaceDecl
                                                      (translateName name)
                                                      ps
                                                      (map translatePTerm
                                                         parentTerms)
                                                      (map (snd . translatePDecl)
                                                         methods))
                           IS.PImplementation vis opts pass implicits constraints
                             name
                             params
                             implName
                             nusing
                             body =>
                             let visibility = translateVisibility vis
                               in AST.DImpl [] visibility
                                    (MkImplDecl (map translateName implName)
                                       (translateName name)
                                       (map translatePTerm params)
                                       (map (map (snd . translatePDecl)) body))
                           IS.PRecord doc vis treq
                             (MkPRecord tyname params opts conName decls) =>
                             let ps = concatMap (translateBasicMultiBinder . bind)
                                        params
                               in let fields = concatMap translatePField decls
                                    in let con = map (translateName . val) conName
                                         in let docs = docToComments doc
                                              in let visibility = translateVisibility
                                                                    (collapseDefault
                                                                       vis)
                                                   in AST.DRecord docs visibility
                                                        (MkRecordDecl
                                                           (translateName tyname)
                                                           ps
                                                           con
                                                           fields)
                           IS.PRecord doc vis treq (MkPRecordLater tyname params) =>
                             AST.DComment
                               (MkComment LineComment "forward record declaration" 0
                                  0)
                           IS.PFail msg decls =>
                             AST.DComment
                               (MkComment LineComment "PFail not yet translated" 0 0)
                           IS.PMutual decls =>
                             AST.DMutual (map (snd . translatePDecl) decls)
                           IS.PFixity fixData =>
                             let ops = map (\op => translateName op.toName)
                                         (forget fixData.operators)
                               in AST.DFixity
                                    (MkFixityDecl (translateFixity fixData.fixity)
                                       fixData.precedence
                                       ops)
                           IS.PNamespace ns decls =>
                             AST.DNamespace [show ns]
                               (map (snd . translatePDecl) decls)
                           IS.PTransform name lhs rhs =>
                             AST.DTransform name (translatePTerm lhs)
                               (translatePTerm rhs)
                           IS.PRunElabDecl tm => AST.DRunElab (translatePTerm tm)
                           IS.PDirective dir =>
                             AST.DDirective (translateDirective dir)
                           IS.PBuiltin bt n =>
                             AST.DBuiltin (show bt) (translateName n)
                in (line, decl)

||| Convert compiler Error to formatter ParseError.
fromError : CC.Error -> ParseError
fromError err = ParseErr (show err)

||| Translate compiler Import to formatter ImportDecl.
translateImport : IS.Import -> AST.ImportDecl
translateImport imp =
  let path = forget (split (== '/') (toPath imp.path))
    in let alias = if show imp.nameAs == show imp.path
                     then Nothing
                     else Just (show imp.nameAs)
         in MkImportDecl imp.reexport path alias Nothing Nothing

||| Split source into lines.
lines' : String -> List String
lines' s = go [] (unpack s)
  where
    go : List Char -> List Char -> List String
    go acc [] = [pack (reverse acc)]
    go acc ('\n' :: rest) = pack (reverse acc) :: go [] rest
    go acc (c :: rest) = go (c :: acc) rest

||| Extract substring from source by 0-based line/column bounds.
extractText : String -> (Int, Int) -> (Int, Int) -> String
extractText src (sl, sc) (el, ec) =
  let ls = lines' src
    in let startLn = cast sl
         in let startCol = cast sc
              in let endLn = cast el
                   in let endCol = cast ec
                        in if startLn == endLn
                             then case L.drop startLn ls of
                                    [] => ""
                                    (l :: _) =>
                                      pack
                                        (take (endCol `minus` startCol)
                                           (drop startCol (unpack l)))
                             else case L.drop startLn ls of
                                    [] => ""
                                    (l :: rest) =>
                                      extractMulti l rest startCol startLn endLn
                                        endCol
  where
    extractMulti : String -> List String -> Nat -> Nat -> Nat -> Nat -> String
    extractMulti first rest sc' sl' el' ec' =
      let f = pack (drop sc' (unpack first))
        in let m = take ((el' `minus` sl') `minus` 1) rest
             in let l = case L.drop ((el' `minus` sl') `minus` 1) rest of
                          [] => ""
                          (l' :: _) => pack (take ec' (unpack l'))
                  in f ++ "\n" ++ unlines m ++ l

||| Strip comment markers from extracted text.
stripComment : String -> (C.CommentStyle, String)
stripComment s =
  let t = S.trim s
    in if isPrefixOf "--" t
         then (C.LineComment, S.trim (substr 2 (length t `minus` 2) t))
         else
           if isPrefixOf "{-" t && isSuffixOf "-}" t
             then (C.BlockComment, S.trim (substr 2 (length t `minus` 4) t))
             else (C.LineComment, t)

||| Check if a comment is a doc comment (starts with |||).
isDocComment : String -> Bool
isDocComment s = isPrefixOf "|||" (S.trim s)

||| Extract comments from parser state.
||| Filters out doc comments (|||) since those are handled via declaration doc fields.
extractComments : String -> PRS.State -> List C.Comment
extractComments src state =
  let decs = state.decorations
    in let commentDecs = filter (\(_, (d, _)) => d == Comment) decs
         in mapMaybe
              (\((_, (start, end)), (_, _)) =>
                 let text = extractText src start end
                   in let (style, content) = stripComment text
                        in if isDocComment text
                             then Nothing
                             else Just
                                    (C.MkComment style content
                                       (cast (fst start))
                                       (cast (snd start))))
              commentDecs

||| Pair a comment with its declaration wrapper and line number.
commentPair : C.Comment -> (Nat, AST.Decl AST.Name)
commentPair c = (c.line, AST.DComment c)

||| Merge declarations and comments by source line number.
mergeByLine : List (Nat, AST.Decl AST.Name)
                -> List (Nat, AST.Decl AST.Name) -> List (Nat, AST.Decl AST.Name)
mergeByLine = L.mergeBy (comparing fst)

||| Check if a declaration pair is a type signature followed by its definition.
isClaimDefPair : AST.Decl AST.Name -> AST.Decl AST.Name -> Bool
isClaimDefPair (AST.DClaim _ _ n1 _ _) (AST.DDef _ n2 _) = n1 == n2
isClaimDefPair _ _ = False

||| Check if a declaration is a comment.
isComment : AST.Decl AST.Name -> Bool
isComment (AST.DComment _) = True
isComment _ = False

||| Insert blank lines between declarations based on line gaps.
insertBlanks : List (Nat, AST.Decl AST.Name) -> List (AST.Decl AST.Name)
insertBlanks [] = []
insertBlanks [(_, d)] = [d]
insertBlanks ((l1, d1) :: (l2, d2) :: rest) =
  if l2 > l1 + 1 && not (isClaimDefPair d1 d2) && not
                                                    (isComment
                                                       d1 || isComment d2)
    then d1 :: AST.DBlank 1 :: insertBlanks ((l2, d2) :: rest)
    else d1 :: insertBlanks ((l2, d2) :: rest)

||| Parse a full module from source text.
||| Uses Idris2's built-in parser.
export parseModule : String -> Either ParseError (List (AST.Decl AST.Name))
parseModule src =
  let origin = CFC.Virtual CFC.Interactive
    in let result = PS.runParser origin Nothing src (IP.prog origin)
         in case result of
              Left err => Left (fromError err)
              Right (_, (state, mod)) =>
                let modName = show (IS.Module.moduleNS mod)
                  in let header = (0, AST.DModule modName [])
                       in let imps = map
                                       (\imp =>
                                          (0, AST.DImport (translateImport imp)))
                                       (IS.Module.imports mod)
                            in let decls = map translatePDecl
                                             (IS.Module.decls mod)
                                 in let comments = map commentPair
                                                     (extractComments src state)
                                      in let merged = mergeByLine
                                                        (header :: imps ++ decls)
                                                        comments
                                           in let withBlanks = insertBlanks
                                                                 merged
                                                in Right withBlanks

||| Parse a single expression from source text.
export parseExpr : String -> Either ParseError (AST.Expr AST.Name)
parseExpr src = let origin = CFC.Virtual CFC.Interactive
                  in let result = PS.runParser origin Nothing src
                                    (IP.expr IP.pdef origin PRS.init)
                       in case result of
                            Left err => Left (fromError err)
                            Right (_, (_, tm)) => Right (translatePTerm tm)
