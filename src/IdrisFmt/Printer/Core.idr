module IdrisFmt.Printer.Core

import public Idris.Syntax as IS
import public Core.Name as CN
import public Core.FC as CFC
import public Algebra.ZeroOneOmega
import public Libraries.Data.WithDefault
import Data.List as L
import Data.List1
import Data.String as S
import Text.PrettyPrint.Bernardy as PP
import Text.PrettyPrint.Bernardy.Combinators as PPC
import IdrisFmt.Doc as D

%default covering

||| Helper to render a Name to String for Doc.
nameStr : Name -> String
nameStr n = show n

||| Check if a string is an operator symbol.
isOpSymbol : String -> Bool
isOpSymbol s =
  let chars = unpack s
  in case chars of
       [] => False
       _ => all (\c => c `elem` unpack "!#$%&*+./<=>?@\\^|-~:") chars

||| Pretty-print a Name, parenthesizing if it's an operator.
prettyNameOp : {opts : _} -> Name -> Doc opts
prettyNameOp n =
  let s = nameStr n
  in if isOpSymbol s
       then parens (line s)
       else line s

||| Pretty-print an Idris2 Name.
export
prettyName : {opts : _} -> Name -> Doc opts
prettyName n = line (nameStr n)

||| Pretty-print a RigCount.
export
prettyRig : {opts : _} -> RigCount -> Doc opts
prettyRig rig = case show rig of
  "0" => line "0 "
  "1" => line "1 "
  _   => empty

||| Pretty-print a constant.
export
prettyConstant : {opts : _} -> Constant -> Doc opts
prettyConstant c = line (show c)

||| Punctuate a list of documents with a separator.
punctuate : {opts : _} -> Doc opts -> List (Doc opts) -> List (Doc opts)
punctuate _ [] = []
punctuate _ [d] = [d]
punctuate p (d :: ds) = (d <+> p) :: punctuate p ds

Prec' : Type
Prec' = Nat

startPrec : Prec'
startPrec = 0

arrowPrec : Prec'
arrowPrec = 5

appPrec : Prec'
appPrec = 10

leftAppPrec : Prec'
leftAppPrec = 9

||| Parenthesise if precedence condition met.
parenthesise' : {opts : _} -> Bool -> Doc opts -> Doc opts
parenthesise' True d = parens d
parenthesise' False d = d

||| Pretty-print an Idris2 OpStr.
prettyOpStr : {opts : _} -> OpStr -> Doc opts
prettyOpStr (OpSymbols n) = prettyName n
prettyOpStr (Backticked n) = line "`" <+> prettyName n <+> line "`"

||| Pretty-print a Directive.
prettyDirective : Directive -> String
prettyDirective (DefaultTotality tot) = "default " ++ show tot
prettyDirective (LazyOn True) = "lazy on"
prettyDirective (LazyOn False) = "lazy off"
prettyDirective (UnboundImplicits True) = "unbound_implicits on"
prettyDirective (UnboundImplicits False) = "unbound_implicits off"
prettyDirective (AmbigDepth n) = "ambiguity_depth " ++ show n
prettyDirective (TotalityDepth n) = "totality_depth " ++ show n
prettyDirective (PrefixRecordProjections True) = "prefix_record_projections on"
prettyDirective (PrefixRecordProjections False) = "prefix_record_projections off"
prettyDirective (AutoImplicitDepth n) = "auto_implicit_depth " ++ show n
prettyDirective (NFMetavarThreshold n) = "nfmetavar_threshold " ++ show n
prettyDirective (SearchTimeout n) = "search_timeout " ++ show n
prettyDirective (CGAction cg act) = "cg " ++ cg ++ " " ++ act
prettyDirective (Extension ext) = "language " ++ show ext
prettyDirective (Overloadable n) = "overloadable " ++ show n
prettyDirective (Names n ns) = "names " ++ show n ++ " " ++ show ns
prettyDirective (StartExpr tm) = "startExpr ..."
prettyDirective (PairNames n1 n2 n3) = "pair " ++ show n1 ++ " " ++ show n2 ++ " " ++ show n3
prettyDirective (RewriteName n1 n2) = "rewrite " ++ show n1 ++ " " ++ show n2
prettyDirective (PrimInteger n) = "integerLit " ++ show n
prettyDirective (PrimString n) = "stringLit " ++ show n
prettyDirective (PrimChar n) = "charLit " ++ show n
prettyDirective (PrimDouble n) = "doubleLit " ++ show n
prettyDirective (PrimTTImp n) = "primTTImp " ++ show n
prettyDirective (PrimName n) = "primName " ++ show n
prettyDirective (PrimDecls n) = "primDecls " ++ show n
prettyDirective (ForeignImpl n tms) = "foreign " ++ show n
prettyDirective (Hide (HideName n)) = "hide " ++ show n
prettyDirective (Hide (HideFixity _ n)) = "hide " ++ show n
prettyDirective (Unhide n) = "unhide " ++ show n
prettyDirective (Logging Nothing) = "logging off"
prettyDirective (Logging (Just _)) = "logging on"
prettyDirective _ = "/* unknown directive */"

||| Render a visibility keyword.
prettyVis : {opts : _} -> Visibility -> Doc opts
prettyVis Private = empty
prettyVis Export = keyword "export"
prettyVis Public = keyword "public" <++> keyword "export"

||| Render visibility with trailing space if not Private.
prettyVisSpace : {opts : _} -> Visibility -> Doc opts
prettyVisSpace Private = empty
prettyVisSpace v = prettyVis v <++> empty

||| Render a BasicMultiBinder.
prettyBasicMultiBinder : {opts : _} -> BasicMultiBinder -> Doc opts
prettyBasicMultiBinder (MkBasicMultiBinder rig names ty) =
  let nameDoc = prettyRig rig <+> hsep (map (prettyName . val) (forget names))
  in case ty of
       PImplicit _ => nameDoc
       PInfer _    => nameDoc
       _           => nameDoc <++> colon <++> line (show ty)

mutual
  ||| Render a PBinder as a parameter doc.
  prettyPBinder : {opts : _} -> PBinder -> Doc opts
  prettyPBinder (MkPBinder Implicit bind) =
    braces (prettyBasicMultiBinder bind)
  prettyPBinder (MkPBinder Explicit bind) =
    parens (prettyBasicMultiBinder bind)
  prettyPBinder (MkPBinder AutoImplicit bind) =
    braces (keyword "auto" <++> prettyBasicMultiBinder bind)
  prettyPBinder (MkPBinder (DefImplicit t) bind) =
    braces (keyword "default" <++> prettyPTerm t <++> prettyBasicMultiBinder bind)

  ||| Pretty-print a PStr (string literal part).
  prettyPStr : {opts : _} -> PStr -> Doc opts
  prettyPStr (StrLiteral _ str) = text str
  prettyPStr (StrInterp _ tm) = line "\\{" <+> prettyPTerm tm <+> line "}"

  ||| Pretty-print a PFieldUpdate.
  prettyPFieldUpdate : {opts : _} -> PFieldUpdate -> Doc opts
  prettyPFieldUpdate (PSetField path v) =
    hsep (map line path) <++> equals <++> prettyPTerm v
  prettyPFieldUpdate (PSetFieldApp path v) =
    hsep (map line path) <++> keyword "$=" <++> prettyPTerm v

  ||| Pretty-print a PDo statement.
  prettyPDo : {opts : _} -> PDo -> Doc opts
  prettyPDo (DoExp _ tm) = prettyPTerm tm
  prettyPDo (DoBind _ _ n rig (Just ty) tm) =
    prettyRig rig <+> prettyName n <++> colon <++> prettyPTerm ty <++> keyword "<-" <++> prettyPTerm tm
  prettyPDo (DoBind _ _ n rig Nothing tm) =
    prettyRig rig <+> prettyName n <++> keyword "<-" <++> prettyPTerm tm
  prettyPDo (DoBindPat _ l (Just ty) tm alts) =
    prettyPTerm l <++> colon <++> keyword "<-" <++> prettyPTerm tm
  prettyPDo (DoBindPat _ l Nothing tm alts) =
    prettyPTerm l <++> keyword "<-" <++> prettyPTerm tm
  prettyPDo (DoLet _ _ l rig ty tm) =
    keyword "let" <++> prettyRig rig <+> prettyName l <++> colon <++> prettyPTerm ty <++> equals <++> prettyPTerm tm
  prettyPDo (DoLetPat _ l ty tm alts) =
    keyword "let" <++> prettyPTerm l <++> equals <++> prettyPTerm tm
  prettyPDo (DoLetLocal _ ds) =
    keyword "let" <++> braces (angles (angles "definitions"))
  prettyPDo (DoRewrite _ rule) =
    keyword "rewrite" <++> prettyPTerm rule

  ||| Pretty-print a PClause for a top-level definition (uses `=`).
  prettyPClauseDef : {opts : _} -> PClause -> Doc opts
  prettyPClauseDef (MkPatClause _ lhs rhs []) =
    prettyPTerm lhs <++> equals <++> prettyPTerm rhs
  prettyPClauseDef (MkPatClause _ lhs rhs ws) =
    prettyPTerm lhs <++> equals
      `vappend` indent 2 (prettyPTerm rhs
        `vappend` indent 2 (keyword "where" `vappend` indent 2 (vsep (map prettyPDecl ws))))
  prettyPClauseDef (MkWithClause _ lhs wps flags _) =
    prettyPTerm lhs <++> keyword "with" <++> parens (hsep (map (prettyPTerm . withRigValue) (forget wps)))
  prettyPClauseDef (MkImpossible _ lhs) =
    prettyPTerm lhs <++> keyword "impossible"

  ||| Pretty-print a PClause for a case alternative (uses `=>`).
  prettyPClauseCase : {opts : _} -> PClause -> Doc opts
  prettyPClauseCase (MkPatClause _ lhs rhs _) =
    prettyPTerm lhs <++> keyword "=>" <++> prettyPTerm rhs
  prettyPClauseCase (MkWithClause _ lhs wps flags _) =
    prettyPTerm lhs <++> keyword "with" <++> parens (hsep (map (prettyPTerm . withRigValue) (forget wps)))
  prettyPClauseCase (MkImpossible _ lhs) =
    prettyPTerm lhs <++> keyword "impossible"

  ||| Pretty-print a PTerm. Main entry point.
  export
  prettyPTerm : {opts : _} -> PTerm -> Doc opts
  prettyPTerm = prettyPrecPTerm startPrec
    where
      prettyPrecPTerm : {opts : _} -> Prec' -> PTerm -> Doc opts
      prettyPrecPTerm d (PRef _ nm) = prettyName nm
      prettyPrecPTerm d (PPi _ rig Explicit Nothing arg ret) =
        parenthesise' (d > arrowPrec) $
          prettyPrecPTerm (arrowPrec + 1) arg <++> line "->" <++> prettyPrecPTerm arrowPrec ret
      prettyPrecPTerm d (PPi _ rig Explicit (Just n) arg ret) =
        parenthesise' (d > arrowPrec) $
          parens (prettyRig rig <+> prettyName n <++> colon <++> prettyPTerm arg)
            <++> line "->" <++> prettyPrecPTerm arrowPrec ret
      prettyPrecPTerm d (PPi _ rig Implicit Nothing arg ret) =
        parenthesise' (d > arrowPrec) $
          braces (prettyRig rig <+> line "_" <++> colon <++> prettyPTerm arg)
            <++> line "->" <++> prettyPrecPTerm arrowPrec ret
      prettyPrecPTerm d (PPi _ rig Implicit (Just n) arg ret) =
        parenthesise' (d > arrowPrec) $
          braces (prettyRig rig <+> prettyName n <++> colon <++> prettyPTerm arg)
            <++> line "->" <++> prettyPrecPTerm arrowPrec ret
      prettyPrecPTerm d (PPi _ rig AutoImplicit Nothing arg ret) =
        parenthesise' (d > arrowPrec) $
          prettyPrecPTerm (arrowPrec + 1) arg <++> line "=>" <++> prettyPrecPTerm arrowPrec ret
      prettyPrecPTerm d (PPi _ rig AutoImplicit (Just n) arg ret) =
        parenthesise' (d > arrowPrec) $
          braces (keyword "auto" <++> prettyRig rig <+> prettyName n <++> colon <++> prettyPTerm arg)
            <++> line "->" <++> prettyPrecPTerm arrowPrec ret
      prettyPrecPTerm d (PPi _ rig (DefImplicit t) Nothing arg ret) =
        parenthesise' (d > arrowPrec) $
          braces (keyword "default" <++> prettyPTerm t <++> prettyRig rig <+> line "_" <++> colon <++> prettyPTerm arg)
            <++> line "->" <++> prettyPrecPTerm arrowPrec ret
      prettyPrecPTerm d (PPi _ rig (DefImplicit t) (Just n) arg ret) =
        parenthesise' (d > arrowPrec) $
          braces (keyword "default" <++> prettyPTerm t <++> prettyRig rig <+> prettyName n <++> colon <++> prettyPTerm arg)
            <++> line "->" <++> prettyPrecPTerm arrowPrec ret
      prettyPrecPTerm d (PLam _ rig _ n ty sc) =
        parenthesise' (d > startPrec) $
          line "\\" <+> prettyRig rig <+> prettyPTerm n <++> line "=>" <++> prettyPTerm sc
      prettyPrecPTerm d (PLet _ rig n ty val sc alts) =
        parenthesise' (d > startPrec) $
          keyword "let" <++> prettyRig rig <+> prettyPTerm n <++> colon <++> prettyPTerm ty <++> equals <++> prettyPTerm val
            <++> keyword "in" <++> prettyPTerm sc
      prettyPrecPTerm d (PCase _ _ tm cs) =
        parenthesise' (d > startPrec) $
          keyword "case" <++> prettyPTerm tm <++> keyword "of" `vappend` indent 2 (vsep (map prettyPClauseCase cs))
      prettyPrecPTerm d (PLocal _ ds sc) =
        parenthesise' (d > startPrec) $
          keyword "let" <++> braces (angles (angles "definitions")) <++> keyword "in" <++> prettyPTerm sc
      prettyPrecPTerm d (PApp _ f a) =
        parenthesise' (d >= appPrec) $ prettyPrecPTerm leftAppPrec f <++> prettyPrecPTerm appPrec a
      prettyPrecPTerm d (PWithApp _ f a) =
        prettyPrecPTerm d f <++> keyword "with" <++> prettyPTerm a
      prettyPrecPTerm d (PNamedApp _ f n a) =
        parenthesise' (d > startPrec) $ prettyPrecPTerm leftAppPrec f <++> braces (prettyName n <++> equals <++> prettyPTerm a)
      prettyPrecPTerm d (PAutoApp _ f a) =
        parenthesise' (d > startPrec) $ prettyPrecPTerm leftAppPrec f <++> "@" <+> braces (prettyPTerm a)
      prettyPrecPTerm d (PPostfixApp _ rec fields) =
        prettyPTerm rec <++> hsep (map (\(fc, n) => line "." <+> prettyName n) fields)
      prettyPrecPTerm d (PPostfixAppPartial _ fields) =
        line "." <+> hsep (map (\(fc, n) => prettyName n) fields)
      prettyPrecPTerm d (PPrefixOp _ op x) =
        parens (prettyOpStr op.val <++> prettyPTerm x)
      prettyPrecPTerm d (PSectionL _ op x) =
        parens (prettyOpStr op.val <++> prettyPTerm x)
      prettyPrecPTerm d (PSectionR _ x op) =
        parens (prettyPTerm x <++> prettyOpStr op.val)
      prettyPrecPTerm d (PEq _ l r) =
        parenthesise' (d >= appPrec) $ prettyPrecPTerm leftAppPrec l <++> equals <++> prettyPrecPTerm leftAppPrec r
      prettyPrecPTerm d (PBracketed _ tm) =
        parens (prettyPTerm tm)
      prettyPrecPTerm d (PString _ _ xs) =
        dquotes (hcat (map prettyPStr xs))
      prettyPrecPTerm d (PMultiline _ _ indent xs) =
        dquotes (hcat (map prettyPStr (concat xs)))
      prettyPrecPTerm d (PList _ _ xs) =
        brackets (hsep (punctuate (line ",") (map (prettyPTerm . snd) xs)))
      prettyPrecPTerm d (PSnocList _ _ xs) =
        line "[<" <+> hsep (punctuate (line ",") (map (prettyPTerm . snd) (xs <>> []))) <+> line "]"
      prettyPrecPTerm d (PPair _ l r) =
        parens (prettyPTerm l <+> line "," <++> prettyPTerm r)
      prettyPrecPTerm d (PDPair _ _ l (PImplicit _) r) =
        parens (prettyPTerm l <++> keyword "**" <++> prettyPTerm r)
      prettyPrecPTerm d (PDPair _ _ l ty r) =
        parens (prettyPTerm l <++> colon <++> prettyPTerm ty <++> keyword "**" <++> prettyPTerm r)
      prettyPrecPTerm d (PUnit _) =
        line "()"
      prettyPrecPTerm d (PDoBlock _ ns stmts) =
        keyword "do" `vappend` indent 2 (vsep (map prettyPDo stmts))
      prettyPrecPTerm d (PBang _ tm) =
        "!" <+> prettyPrecPTerm d tm
      prettyPrecPTerm d (PIdiom _ ns tm) =
        lbracket <+> pipe <+> prettyPTerm tm <+> pipe <+> rbracket
      prettyPrecPTerm d (PIfThenElse _ c t e) =
        parenthesise' (d > startPrec) $
          keyword "if" <++> prettyPTerm c <++> keyword "then" <++> prettyPTerm t <++> keyword "else" <++> prettyPTerm e
      prettyPrecPTerm d (PComprehension _ ret es) =
        brackets (prettyPTerm ret <++> pipe <++> vsep (punctuate (line ",") (map prettyPDo es)))
      prettyPrecPTerm d (PRewrite _ rule tm) =
        keyword "rewrite" <++> prettyPTerm rule <++> keyword "in" <++> prettyPTerm tm
      prettyPrecPTerm d (PRange _ start Nothing end) =
        brackets (prettyPTerm start <++> line ".." <++> prettyPTerm end)
      prettyPrecPTerm d (PRange _ start (Just next) end) =
        brackets (prettyPTerm start <+> line "," <++> prettyPTerm next <++> line ".." <++> prettyPTerm end)
      prettyPrecPTerm d (PRangeStream _ start Nothing) =
        brackets (prettyPTerm start <++> line "..")
      prettyPrecPTerm d (PRangeStream _ start (Just next)) =
        brackets (prettyPTerm start <+> line "," <++> prettyPTerm next <++> line "..")
      prettyPrecPTerm d (PDelayed _ _ ty) =
        prettyPTerm ty
      prettyPrecPTerm d (PDelay _ tm) =
        keyword "Delay" <++> prettyPTerm tm
      prettyPrecPTerm d (PForce _ tm) =
        keyword "Force" <++> prettyPTerm tm
      prettyPrecPTerm d (PType _) =
        keyword "Type"
      prettyPrecPTerm d (PImplicit _) =
        line "_"
      prettyPrecPTerm d (PInfer _) =
        line "?"
      prettyPrecPTerm d (PHole _ _ n) =
        line "?" <+> line n
      prettyPrecPTerm d (PPrimVal _ c) =
        prettyConstant c
      prettyPrecPTerm d (PAs _ _ n p) =
        prettyName n <+> line "@" <+> prettyPTerm p
      prettyPrecPTerm d (PDotted _ p) =
        dot <+> prettyPTerm p
      prettyPrecPTerm d (PQuote _ tm) =
        line "`" <+> parens (prettyPTerm tm)
      prettyPrecPTerm d (PQuoteName _ n) =
        line "`" <+> braces (prettyName n)
      prettyPrecPTerm d (PQuoteDecl _ ds) =
        line "`(" <+> vsep (map prettyPDecl ds) <+> line ")"
      prettyPrecPTerm d (PUnquote _ tm) =
        line "~" <+> parens (prettyPTerm tm)
      prettyPrecPTerm d (PRunElab _ tm) =
        keyword "%runElab" <++> prettyPTerm tm
      prettyPrecPTerm d (PSearch _ _) =
        keyword "%search"
      prettyPrecPTerm d (PUpdate _ fs) =
        keyword "record" <++> braces (vsep (punctuate (line ",") (map prettyPFieldUpdate fs)))
      prettyPrecPTerm d (PWithUnambigNames _ ns rhs) =
        keyword "with" <++> parens (hsep (map (prettyName . snd) ns)) <++> prettyPTerm rhs
      prettyPrecPTerm d (POp _ lhsInfo op rhs) =
        parenthesise' (d >= appPrec) $ prettyPrecPTerm leftAppPrec lhsInfo.val.getLhs <++> prettyOpStr op.val <++> prettyPrecPTerm leftAppPrec rhs
      prettyPrecPTerm d (PUnifyLog _ _ tm) =
        prettyPTerm tm
      prettyPrecPTerm d (NewPi x) =
        let binder = x.val.binder
            scope = x.val.scope
        in parenthesise' (d > arrowPrec) $
             prettyPBinder binder <++> line "->" <++> prettyPTerm scope
      prettyPrecPTerm d (Forall x) =
        let names = fst x.val
            scope = snd x.val
        in parenthesise' (d > startPrec) $
             keyword "forall" <++> hsep (map (prettyName . val) (forget names)) <++> line "." <++> prettyPTerm scope

  ||| Pretty-print a PDecl.
  export
  prettyPDecl : {opts : _} -> PDecl -> Doc opts
  prettyPDecl (MkWithData fc (PClaim claimData)) =
    let ty = claimData.type
        ns = map prettyName ty.nameList
    in hsep ns <++> colon <++> prettyPTerm ty.val.type
  prettyPDecl (MkWithData fc (PDef clauses)) =
    vsep (map prettyPClauseDef clauses)
  prettyPDecl (MkWithData fc (PData doc vis treq decl)) =
    case decl of
      MkPData _ tyname tycon opts datacons =>
        let visDoc = prettyVisSpace (collapseDefault vis)
            tyconDoc = case tycon of
                         Nothing => prettyName tyname
                         Just t => prettyName tyname <++> colon <++> prettyPTerm t
            header = keyword "data" <++> visDoc <+> tyconDoc <++> keyword "where"
            cons = case datacons of
                     [] => empty
                     cs => indent 2 (vsep (map prettyConDecl cs))
        in header `vappend` cons
      MkPLater _ tyname tycon =>
        keyword "data" <++> prettyName tyname <++> colon <++> prettyPTerm tycon
    where
      prettyConDecl : {opts : _} -> PTypeDecl -> Doc opts
      prettyConDecl ty =
        let td = val ty
            ns = map (val . snd) (forget td.names)
        in case ns of
             [] => prettyName (UN (Basic "unnamed")) <++> colon <++> prettyPTerm td.type
             (n :: _) => prettyNameOp n <++> colon <++> prettyPTerm td.type
  prettyPDecl (MkWithData fc (PParameters params decls)) =
    let paramDocs = case params of
                      Left plainBinds =>
                        hsep (map (\b => prettyName b.nameVal <++> colon <++> prettyPTerm b.val) (forget plainBinds))
                      Right pbBinds =>
                        hsep (map prettyPBinder (forget pbBinds))
    in keyword "parameters" <++> paramDocs
      `vappend` indent 2 (vsep (map prettyPDecl decls))
  prettyPDecl (MkWithData fc (PUsing usings decls)) =
    keyword "using" <++> parens (hsep (map prettyUsing usings))
      <++> keyword "where" `vappend` indent 2 (vsep (map prettyPDecl decls))
    where
      prettyUsing : {opts : _} -> (Maybe Name, PTerm) -> Doc opts
      prettyUsing (Nothing, ty) = prettyPTerm ty
      prettyUsing (Just n, ty) = prettyName n <++> colon <++> prettyPTerm ty
  prettyPDecl (MkWithData fc (PInterface vis constraints name doc params det conName methods)) =
    let visDoc = prettyVisSpace (collapseDefault vis)
        constraintDocs = case constraints of
                           [] => empty
                           cs => parens (hsep (punctuate (line ",") (map (prettyPTerm . snd) cs))) <++> keyword "=>" <++> empty
        paramDocs = case params of
                      [] => empty
                      ps => hsep (map prettyBasicMultiBinder ps)
        header = keyword "interface" <++> visDoc <+> constraintDocs <+> prettyName name <++> paramDocs <++> keyword "where"
        body = case methods of
                 [] => empty
                 ms => indent 2 (vsep (map prettyPDecl ms))
    in header `vappend` body
  prettyPDecl (MkWithData fc (PImplementation vis opts pass implicits constraints name params implName nusing body)) =
    let visDoc = case vis of
                   Private => empty
                   _ => prettyVis vis `vappend` empty
        implDoc = case implName of
                    Nothing => empty
                    Just n => prettyName n <++> equals <++> empty
        constraintDocs = case constraints of
                           [] => empty
                           cs => parens (hsep (punctuate (line ",") (map (prettyPTerm . snd) cs))) <++> keyword "=>" <++> empty
        paramDocs = case params of
                      [] => empty
                      ps => hsep (map prettyPTerm ps)
        header = keyword "implementation" <++> implDoc <+> constraintDocs <+> prettyName name <++> paramDocs
    in mkImplResult visDoc header body
    where
      mkImplResult : {opts : _} -> Doc opts -> Doc opts -> Maybe (List PDecl) -> Doc opts
      mkImplResult vd hdr Nothing = vd `vappend` hdr
      mkImplResult vd hdr (Just ds) =
        let bodyDoc = indent 2 (vsep (map prettyPDecl ds))
        in vd `vappend` (hdr <++> keyword "where" `vappend` bodyDoc)
  prettyPDecl (MkWithData fc (PFixity fixData)) =
    let fixStr = case fixData.fixity of
                   InfixL => "infixl"
                   InfixR => "infixr"
                   Infix  => "infix"
                   Prefix => "prefix"
        ops = map prettyOpStr (forget fixData.operators)
    in keyword fixStr <++> line (show fixData.precedence) <++> hsep ops
  prettyPDecl (MkWithData fc (PRecord doc vis treq decl)) =
    case decl of
      MkPRecord tyname params opts conName decls =>
        let visDoc = prettyVisSpace (collapseDefault vis)
            paramDocs = case params of
                          [] => empty
                          ps => hsep (map prettyPBinder ps)
            header = keyword "record" <++> visDoc <+> prettyName tyname <++> paramDocs <++> keyword "where"
            conDoc = case conName of
                       Nothing => []
                       Just c => [keyword "constructor" <++> prettyName c.val]
            fieldDocs = map prettyFieldDecl decls
            allBody = conDoc ++ fieldDocs
            body = case allBody of
                     [] => empty
                     bs => indent 2 (vsep bs)
        in header `vappend` body
      MkPRecordLater tyname params =>
        keyword "record" <++> prettyName tyname
    where
      prettyFieldDecl : {opts : _} -> PField -> Doc opts
      prettyFieldDecl f =
        let rig = f.rig
            names = f.names
            ty = (val f).boundType
        in prettyRig rig <+> hsep (map (prettyName . val) names) <++> colon <++> prettyPTerm ty
  prettyPDecl (MkWithData fc (PMutual decls)) =
    keyword "mutual" `vappend` indent 2 (vsep (map prettyPDecl decls))
  prettyPDecl (MkWithData fc (PNamespace ns decls)) =
    keyword "namespace" <++> line (show ns) `vappend` indent 2 (vsep (map prettyPDecl decls))
  prettyPDecl (MkWithData fc (PTransform _ _ _)) =
    line "/* transform declaration */"
  prettyPDecl (MkWithData fc (PRunElabDecl tm)) =
    keyword "%runElab" <++> prettyPTerm tm
  prettyPDecl (MkWithData fc (PDirective dir)) =
    keyword "%" <+> line (prettyDirective dir)
  prettyPDecl (MkWithData fc (PBuiltin bt n)) =
    keyword "%builtin" <++> line (show bt) <++> prettyName n
  prettyPDecl (MkWithData fc (PFail str decls)) =
    line "/* failed declaration */"

  ||| Pretty-print an Import.
  export
  prettyImport : {opts : _} -> Import -> Doc opts
  prettyImport (MkImport _ reexport path nameAs) =
    if show nameAs /= show path
      then (if reexport then keyword "public" <++> keyword "import" <++> line (show path) else keyword "import" <++> line (show path))
             <++> keyword "as" <++> line (show nameAs)
      else if reexport then keyword "public" <++> keyword "import" <++> line (show path) else keyword "import" <++> line (show path)

  ||| Pretty-print a full Module.
  export
  prettyModule : {opts : _} -> Module -> Doc opts
  prettyModule mod =
    let header = keyword "module" <++> line (show (IS.Module.moduleNS mod))
        imps = vsep (map prettyImport (IS.Module.imports mod))
        decls = vsep (map prettyPDecl (IS.Module.decls mod))
    in vsep [header, line "", imps, line "", decls]
