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
import IdrisFmt.Comments as C

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

mutual
  ||| Render a BasicMultiBinder.
  prettyBasicMultiBinder : {opts : _} -> BasicMultiBinder -> Doc opts
  prettyBasicMultiBinder (MkBasicMultiBinder rig names ty) =
    let nameDoc = prettyRig rig <+> hsep (map (prettyName . val) (forget names))
    in case ty of
         PImplicit _ => nameDoc
         PInfer _    => nameDoc
         _           => nameDoc <++> colon <++> prettyPTerm ty

  ||| Render a BasicMultiBinder, always including the type annotation.
  prettyBasicMultiBinderFull : {opts : _} -> BasicMultiBinder -> Doc opts
  prettyBasicMultiBinderFull (MkBasicMultiBinder rig names ty) =
    let nameDoc = prettyRig rig <+> hsep (map (prettyName . val) (forget names))
    in nameDoc <++> colon <++> prettyPTerm ty

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

  ||| Render a PBinder as a parameter doc, always including the type annotation.
  prettyPBinderFull : {opts : _} -> PBinder -> Doc opts
  prettyPBinderFull (MkPBinder Implicit bind) =
    braces (prettyBasicMultiBinderFull bind)
  prettyPBinderFull (MkPBinder Explicit bind) =
    parens (prettyBasicMultiBinderFull bind)
  prettyPBinderFull (MkPBinder AutoImplicit bind) =
    braces (keyword "auto" <++> prettyBasicMultiBinderFull bind)
  prettyPBinderFull (MkPBinder (DefImplicit t) bind) =
    braces (keyword "default" <++> prettyPTerm t <++> prettyBasicMultiBinderFull bind)

  ||| Render a PBinder for record parameters (no parens for explicit implicit-typed params).
  prettyRecordParam : {opts : _} -> PBinder -> Doc opts
  prettyRecordParam (MkPBinder Explicit bind) =
    case bind.type of
      PImplicit _ => prettyBasicMultiBinder bind
      PInfer _    => prettyBasicMultiBinder bind
      _           => parens (prettyBasicMultiBinder bind)
  prettyRecordParam (MkPBinder Implicit bind) =
    braces (prettyBasicMultiBinder bind)
  prettyRecordParam (MkPBinder AutoImplicit bind) =
    braces (keyword "auto" <++> prettyBasicMultiBinder bind)
  prettyRecordParam (MkPBinder (DefImplicit t) bind) =
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
  ||| Pretty-print a PClause alternative for do/let pattern bindings.
  prettyPDoAlt : {opts : _} -> PClause -> Doc opts
  prettyPDoAlt (MkPatClause _ lhs rhs _) =
    line "|" <++> prettyPTerm lhs <++> keyword "=>" <++> prettyPTerm rhs
  prettyPDoAlt (MkImpossible _ lhs) =
    line "|" <++> prettyPTerm lhs <++> keyword "impossible"
  prettyPDoAlt (MkWithClause _ lhs wps flags _) =
    line "|" <++> prettyPTerm lhs <++> keyword "with" <++> parens (hsep (map (prettyPTerm . withRigValue) (forget wps)))

  prettyPDo : {opts : _} -> PDo -> Doc opts
  prettyPDo (DoExp _ tm) = prettyPTerm tm
  prettyPDo (DoBind _ _ n rig (Just ty) tm) =
    prettyRig rig <+> prettyName n <++> colon <++> prettyPTerm ty <++> keyword "<-" <++> prettyPTerm tm
  prettyPDo (DoBind _ _ n rig Nothing tm) =
    prettyRig rig <+> prettyName n <++> keyword "<-" <++> prettyPTerm tm
  prettyPDo (DoBindPat _ l (Just ty) tm alts) =
    let bindDoc = prettyPTerm l <++> colon <++> keyword "<-" <++> prettyPTerm tm
        altDocs = case alts of
                    [] => bindDoc
                    _  => bindDoc `vappend` vsep (map prettyPDoAlt alts)
    in altDocs
  prettyPDo (DoBindPat _ l Nothing tm alts) =
    let bindDoc = prettyPTerm l <++> keyword "<-" <++> prettyPTerm tm
        altDocs = case alts of
                    [] => bindDoc
                    _  => bindDoc `vappend` vsep (map prettyPDoAlt alts)
    in altDocs
  prettyPDo (DoLet _ _ l rig ty tm) =
    keyword "let" <++> prettyRig rig <+> prettyName l <++> colon <++> prettyPTerm ty <++> equals <++> prettyPTerm tm
  prettyPDo (DoLetPat _ l ty tm alts) =
    let letDoc = keyword "let" <++> prettyPTerm l <++> equals <++> prettyPTerm tm
        altDocs = case alts of
                    [] => letDoc
                    _  => letDoc `vappend` vsep (map prettyPDoAlt alts)
    in altDocs
  prettyPDo (DoLetLocal _ ds) =
    keyword "let" <++> braces (angles (angles "definitions"))
  prettyPDo (DoRewrite _ rule) =
    keyword "rewrite" <++> prettyPTerm rule

  ||| Pretty-print a PClause for a top-level definition (uses `=`).
  prettyPClauseDef : {opts : _} -> PClause -> Doc opts
  prettyPClauseDef (MkPatClause _ lhs rhs []) =
    case rhs of
      PDoBlock _ _ stmts =>
        (prettyPTerm lhs <++> equals <++> keyword "do") `vappend` indent 2 (vsep (map prettyPDo stmts))
      PIfThenElse _ c t e =>
        (prettyPTerm lhs <++> equals) `vappend` indent 2 (verticalIf c t e)
      _ =>
        (prettyPTerm lhs <++> equals) `vappend` indent 2 (prettyPTerm rhs)
    where
      verticalIf : {opts : _} -> PTerm -> PTerm -> PTerm -> Doc opts
      verticalIf c t e =
        (keyword "if" <++> prettyPTerm c) `vappend` indent 2 ((keyword "then" <++> prettyPTerm t) `vappend` (keyword "else" `vappend` indent 2 (prettyPTerm e)))
  prettyPClauseDef (MkPatClause _ lhs rhs ws) =
    prettyPTerm lhs <++> equals
      `vappend` indent 2 (prettyPTerm rhs
        `vappend` (keyword "where" `vappend` indent 2 (vsep (map prettyPDecl ws))))
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
      prettyPrecPTerm d (PRef _ nm) =
        case nm of
          UN (Field _) => parens (prettyName nm)
          _ => if CN.isOpName nm then parens (prettyName nm) else prettyName nm
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
        case tryLamCase n sc of
          Just doc => doc
          Nothing =>
            case tryTupleSection n sc of
              Just doc => doc
              Nothing =>
                parenthesise' (d > startPrec) $
                  line "\\" <+> prettyRig rig <+> prettyPTerm n <++> line "=>" <++> prettyPTerm sc
        where
          ||| Extract name from a PRef.
          getRefName : PTerm -> Maybe Name
          getRefName (PRef _ n') = Just n'
          getRefName _ = Nothing

          ||| Check if a name is a lambda-case variable.
          isLamCaseVarName : Name -> Bool
          isLamCaseVarName (MN name _) = S.isPrefixOf "lcase" name
          isLamCaseVarName _ = False

          ||| Try to resugar a lambda-case from a lambda.
          tryLamCase : {opts : _} -> PTerm -> PTerm -> Maybe (Doc opts)
          tryLamCase pat body =
            do varName <- getRefName pat
               guard (isLamCaseVarName varName)
               case body of
                 PCase _ _ (PRef _ scrutName) clauses =>
                   if scrutName == varName
                     then Just (line "\\" <+> keyword "case" `vappend` indent 2 (vsep (map prettyPClauseCase clauses)))
                     else Nothing
                 _ => Nothing
          ||| Check if a name is a tuple section variable.
          isSectionVarName : Name -> Bool
          isSectionVarName (MN name _) =
            S.isPrefixOf "__leftTupleSection" name || S.isPrefixOf "__infixTupleSection" name
          isSectionVarName _ = False

          ||| Check if a term is a reference to one of the given section variables.
          isSectionVar : PTerm -> List Name -> Bool
          isSectionVar (PRef _ n') vars = n' `elem` vars
          isSectionVar _ _ = False

          ||| Build tuple section syntax from a pair tree and section variables.
          buildTupleSection : {opts : _} -> List Name -> PTerm -> Maybe (Doc opts)
          buildTupleSection vars (PPair _ l r) =
            let lIsVar = isSectionVar l vars
                rIsVar = isSectionVar r vars
                lDoc = prettyPTerm l
                rDoc = prettyPTerm r
            in case (lIsVar, rIsVar) of
                  (True, True) => Just (parens (line ","))
                  (True, False) => Just (parens (line "," <+> rDoc))
                  (False, True) => Just (parens (lDoc <+> line ","))
                  (False, False) => Nothing
          buildTupleSection vars (PLam _ _ _ pat _ sc') =
            case getRefName pat of
              Just n' =>
                if isSectionVarName n'
                  then buildTupleSection (n' :: vars) sc'
                  else Nothing
              Nothing => Nothing
          buildTupleSection _ _ = Nothing

          ||| Try to resugar a tuple section from a lambda.
          tryTupleSection : {opts : _} -> PTerm -> PTerm -> Maybe (Doc opts)
          tryTupleSection pat body =
            do varName <- getRefName pat
               guard (isSectionVarName varName)
               buildTupleSection [varName] body
      prettyPrecPTerm d (PLet _ rig n ty val sc alts) =
        parenthesise' (d > startPrec) $
          let nameDoc = prettyRig rig <+> prettyPTerm n
              valDoc = equals <++> prettyPTerm val
              fullDoc = case ty of
                          PImplicit _ => nameDoc <++> valDoc
                          PInfer _    => nameDoc <++> valDoc
                          _           => nameDoc <++> colon <++> prettyPTerm ty <++> valDoc
              letBody = case alts of
                          [] => fullDoc
                          _  => fullDoc `vappend` vsep (map prettyAlt alts)
          in keyword "let" <++> letBody <++> keyword "in" <++> prettyPTerm sc
        where
          prettyAlt : {opts : _} -> PClause -> Doc opts
          prettyAlt (MkPatClause _ lhs rhs _) =
            line "|" <++> prettyPTerm lhs <++> keyword "=>" <++> prettyPTerm rhs
          prettyAlt (MkImpossible _ lhs) =
            line "|" <++> prettyPTerm lhs <++> keyword "impossible"
          prettyAlt (MkWithClause _ lhs wps flags _) =
            line "|" <++> prettyPTerm lhs <++> keyword "with" <++> parens (hsep (map (prettyPTerm . withRigValue) (forget wps)))
      prettyPrecPTerm d (PCase _ _ tm cs) =
        parenthesise' (d > startPrec) $
          keyword "case" <++> prettyPTerm tm <++> keyword "of" `vappend` indent 2 (vsep (map prettyPClauseCase cs))
      prettyPrecPTerm d (PLocal _ ds sc) =
        parenthesise' (d > startPrec) $
          (keyword "let" `vappend` indent 2 (vsep (map prettyPDecl ds))) `vappend` keyword "in" <++> prettyPTerm sc
      prettyPrecPTerm d (PApp _ f a) =
        parenthesise' (d >= appPrec) $ prettyPrecPTerm leftAppPrec f <++> prettyPrecPTerm appPrec a
      prettyPrecPTerm d (PWithApp _ f a) =
        prettyPrecPTerm d f <++> keyword "with" <++> prettyPTerm a
      prettyPrecPTerm d (PNamedApp _ f n a) =
        parenthesise' (d > startPrec) $ prettyPrecPTerm leftAppPrec f <++> braces (prettyName n <++> equals <++> prettyPTerm a)
      prettyPrecPTerm d (PAutoApp _ f a) =
        parenthesise' (d > startPrec) $ prettyPrecPTerm leftAppPrec f <++> "@" <+> braces (prettyPTerm a)
      prettyPrecPTerm d (PPostfixApp _ rec fields) =
        prettyPTerm rec <+> hcat (map (\(fc, n) => prettyName n) fields)
      prettyPrecPTerm d (PPostfixAppPartial _ fields) =
        parens (hcat (map (\(fc, n) => prettyName n) fields))
      prettyPrecPTerm d (PPrefixOp _ op x) =
        prettyOpStr op.val <++> prettyPTerm x
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
          ifMultiline compact vertical
        where
          compact : {opts : _} -> Doc opts
          compact = keyword "if" <++> prettyPTerm c <++> keyword "then" <++> prettyPTerm t <++> keyword "else" <++> prettyPTerm e
          vertical : {opts : _} -> Doc opts
          vertical = (keyword "if" <++> prettyPTerm c) `vappend` indent 2 ((keyword "then" <++> prettyPTerm t) `vappend` indent 2 (keyword "else" `vappend` indent 2 (prettyPTerm e)))
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
      prettyPrecPTerm d (PDotted _ p) =
        dot <+> prettyPTerm p
      prettyPrecPTerm d (PAs _ _ n p) =
        prettyName n <+> line "@" <+> prettyPTerm p
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
              prettyPBinderFull binder <++> line "->" <++> prettyPTerm scope
      prettyPrecPTerm d (Forall x) =
        let names = fst x.val
            scope = snd x.val
        in parenthesise' (d > startPrec) $
             keyword "forall" <++> hsep (map (prettyName . val) (forget names)) <++> line "." <++> prettyPTerm scope

  ||| Pretty-print a PDecl.
  export
  prettyPDecl : {opts : _} -> PDecl -> Doc opts
  prettyPDecl (MkWithData fc (PClaim (MkPClaim rig vis opts ty))) =
    let ns = map prettyNameOp ty.nameList
        sigDoc = hsep ns <++> colon <++> prettyPTerm ty.val.type
    in case vis of
         Private => sigDoc
         _       => prettyVis vis `vappend` sigDoc
  prettyPDecl (MkWithData fc (PDef clauses)) =
    vsep (map prettyPClauseDef clauses)
  prettyPDecl (MkWithData fc (PData doc vis treq decl)) =
    case decl of
      MkPData _ tyname tycon opts datacons =>
        let tyconDoc = case tycon of
                         Nothing => prettyName tyname
                         Just t => prettyName tyname <++> colon <++> prettyPTerm t
            header = keyword "data" <++> tyconDoc <++> keyword "where"
            cons = case datacons of
                     [] => empty
                     cs => indent 2 (vsep (map prettyConDecl cs))
        in case collapseDefault vis of
             Private => header `vappend` cons
             _ => (prettyVis (collapseDefault vis) `vappend` header) `vappend` cons
      MkPLater _ tyname tycon =>
        case collapseDefault vis of
          Private => keyword "data" <++> prettyName tyname <++> colon <++> prettyPTerm tycon
          _ => prettyVis (collapseDefault vis) `vappend` keyword "data" <++> prettyName tyname <++> colon <++> prettyPTerm tycon
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
    let constraintDocs = case constraints of
                           [] => empty
                           cs => parens (hsep (punctuate (line ",") (map (prettyPTerm . snd) cs))) <++> keyword "=>"
        paramDocs = case params of
                      [] => empty
                      ps => hsep (map prettyBasicMultiBinder ps)
        header = case (constraints, params) of
                   ([], []) => keyword "interface" <++> prettyName name <++> keyword "where"
                   ([], _)  => keyword "interface" <++> prettyName name <++> paramDocs <++> keyword "where"
                   (_, [])  => keyword "interface" <++> constraintDocs <++> prettyName name <++> keyword "where"
                   (_, _)   => keyword "interface" <++> constraintDocs <++> prettyName name <++> paramDocs <++> keyword "where"
        body = case methods of
                 [] => empty
                 ms => indent 2 (vsep (map prettyPDecl ms))
    in case collapseDefault vis of
         Private => header `vappend` body
         _ => (prettyVis (collapseDefault vis) `vappend` header) `vappend` body
  prettyPDecl (MkWithData fc (PImplementation vis opts pass implicits constraints name params implName nusing body)) =
    let implDoc = case implName of
                    Nothing => empty
                    Just n => prettyName n <++> equals <++> empty
        constraintDocs = case constraints of
                           [] => empty
                           cs => parens (hsep (punctuate (line ",") (map (prettyPTerm . snd) cs))) <++> keyword "=>" <++> empty
        paramDocs = case params of
                      [] => empty
                      ps => hsep (map prettyPTerm ps)
    in mkImplResult vis implDoc constraintDocs paramDocs name body
    where
      mkImplResult : {opts : _} -> Visibility -> Doc opts -> Doc opts -> Doc opts -> Name -> Maybe (List PDecl) -> Doc opts
      mkImplResult vis implDoc' cd pd nm Nothing =
        let header = keyword "implementation" <++> implDoc' <+> cd <+> prettyName nm <++> pd
        in case vis of
             Private => header
             _ => prettyVis vis `vappend` header
      mkImplResult vis implDoc' cd pd nm (Just ds) =
        let header = keyword "implementation" <++> implDoc' <+> cd <+> prettyName nm <++> pd
            bodyDoc = indent 2 (vsep (map prettyPDecl ds))
        in case vis of
             Private => header <++> keyword "where" `vappend` bodyDoc
             _ => (prettyVis vis `vappend` (header <++> keyword "where")) `vappend` bodyDoc
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
      MkPRecord tyname params recOpts recConName recDecls =>
        case collapseDefault vis of
          Private => mkRecordBody tyname params recConName recDecls
          _ => prettyVis (collapseDefault vis) `vappend` mkRecordBody tyname params recConName recDecls
      MkPRecordLater tyname params =>
        let paramDocs = case params of
                          [] => empty
                          ps => hsep (map prettyRecordParam ps)
        in case collapseDefault vis of
             Private => case params of
                          [] => keyword "record" <++> prettyName tyname
                          _  => keyword "record" <++> prettyName tyname <++> paramDocs
             _ => case params of
                     [] => prettyVis (collapseDefault vis) `vappend` keyword "record" <++> prettyName tyname
                     _  => prettyVis (collapseDefault vis) `vappend` keyword "record" <++> prettyName tyname <++> paramDocs
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

  prettyFieldDecl : {opts : _} -> PField -> Doc opts
  prettyFieldDecl f =
    let rig = f.rig
        names = f.names
        ty = (val f).boundType
    in prettyRig rig <+> hsep (map (prettyName . val) names) <++> colon <++> prettyPTerm ty

  mkRecordBody : {opts : _} -> Name -> List PBinder -> Maybe (WithDoc (AddFC Name)) -> List PField -> Doc opts
  mkRecordBody tyname params recConName recDecls =
    let paramDocs = case params of
                      [] => empty
                      ps => hsep (map prettyRecordParam ps)
        header = case params of
                   [] => keyword "record" <++> prettyName tyname <++> keyword "where"
                   _  => keyword "record" <++> prettyName tyname <++> paramDocs <++> keyword "where"
        conDoc = case recConName of
                   Nothing => []
                   Just c => [keyword "constructor" <++> prettyName c.val]
        fieldDocs = map prettyFieldDecl recDecls
        allBody = conDoc ++ fieldDocs
    in case allBody of
         [] => header
         (d :: ds) => header `vappend` indent 2 (vsep (d :: ds))

  ||| Pretty-print an Import.
  export
  prettyImport : {opts : _} -> Import -> Doc opts
  prettyImport (MkImport _ reexport path nameAs) =
    if show nameAs /= show path
      then (if reexport then keyword "import" <++> keyword "public" <++> line (show path) else keyword "import" <++> line (show path))
             <++> keyword "as" <++> line (show nameAs)
      else if reexport then keyword "import" <++> keyword "public" <++> line (show path) else keyword "import" <++> line (show path)

  ||| Extract the primary name from a declaration for grouping.
  declName : PDecl -> Maybe Name
  declName (MkWithData _ (PClaim claimData)) =
    case claimData.type.nameList of
      [] => Nothing
      (n :: _) => Just n
  declName (MkWithData _ (PDef [])) = Nothing
  declName (MkWithData _ (PDef (MkPatClause _ lhs _ _ :: _))) =
    extractAppName lhs
  declName (MkWithData _ (PDef (MkWithClause _ lhs _ _ _ :: _))) =
    extractAppName lhs
  declName (MkWithData _ (PDef (MkImpossible _ lhs :: _))) =
    extractAppName lhs
  declName _ = Nothing

  ||| Extract the function name from a PTerm (recursively through PApp).
  extractAppName : PTerm -> Maybe Name
  extractAppName (PRef _ n) = Just n
  extractAppName (PApp _ f _) = extractAppName f
  extractAppName (PAs _ _ n _) = Just n
  extractAppName _ = Nothing

  ||| Extract start line from FC.
  fcStartLine : FC -> Nat
  fcStartLine (MkFC _ start _) = cast (fst start)
  fcStartLine (MkVirtualFC _ start _) = cast (fst start)
  fcStartLine EmptyFC = 0

  ||| Extract start line from a declaration.
  declLine : PDecl -> Nat
  declLine d = fcStartLine (WithData.get "fc" d)

  ||| Pretty-print a Comment.
  prettyComment : {opts : _} -> C.Comment -> Doc opts
  prettyComment (C.MkComment C.LineComment content _ _) =
    line ("-- " ++ content)
  prettyComment (C.MkComment C.BlockComment content _ _) =
    let ls = S.lines content
    in case ls of
         [] => line "{- -}"
         [single] => line ("{- " ++ single ++ " -}")
         (l :: ls') => vsep (line ("{- " ++ l) :: go ls')
    where
      go : List String -> List (Doc opts)
      go [] = []
      go [last] = [line (last ++ " -}")]
      go (m :: rest) = line m :: go rest
  prettyComment (C.MkComment C.DocComment content _ _) =
    line ("||| " ++ content)

  ||| A module item is either a declaration or a comment.
  data Item = IDecl PDecl | IComment C.Comment

  ||| Extract line number from an item.
  itemLine : Item -> Nat
  itemLine (IDecl d) = declLine d
  itemLine (IComment c) = c.line

  ||| Compare items by line number.
  itemCompare : Item -> Item -> Ordering
  itemCompare x y = compare (itemLine x) (itemLine y)

  ||| Render a single module item.
  prettyItem : {opts : _} -> Item -> Doc opts
  prettyItem (IDecl d) = prettyPDecl d
  prettyItem (IComment c) = prettyComment c

  ||| Decide if we need a blank line between two consecutive items.
  needsBlank : Item -> Item -> Bool
  needsBlank (IComment _) _ = False
  needsBlank _ (IComment _) = False
  needsBlank (IDecl d1) (IDecl d2) =
    not (declName d1 == declName d2 && declName d1 /= Nothing)

  ||| Interleave items with blank lines where needed.
  interleaveItems : {opts : _} -> List Item -> List (Doc opts)
  interleaveItems [] = []
  interleaveItems [item] = [prettyItem item]
  interleaveItems (item1 :: item2 :: rest) =
    if needsBlank item1 item2
      then prettyItem item1 :: line "" :: interleaveItems (item2 :: rest)
      else prettyItem item1 :: interleaveItems (item2 :: rest)

  ||| Check if a comment should be kept as a top-level item.
  ||| Check if a comment should be kept as a top-level item.
  ||| Comments deep inside function bodies (far from declaration start) are skipped.
  keepComment : List Nat -> C.Comment -> Bool
  keepComment declLines comment =
    case declLines of
      [] => True
      (d :: ds) =>
        let prevDecls = filter (<= comment.line) declLines
            nextDecls = filter (> comment.line) declLines
        in case prevDecls of
             [] => True
             (pd :: pds) =>
               let prevDecl = last (pd :: pds)
               in case nextDecls of
                    [] => True
                    _  =>
                       let distance = comment.line `minus` prevDecl
                       in distance <= 3
  ||| Merge declarations and comments by source line.
  mergeItems : List PDecl -> List C.Comment -> List Item
  mergeItems decls comments =
    let declLines = map declLine (L.sortBy (comparing declLine) decls)
        filteredComments = filter (keepComment declLines) comments
    in L.sortBy itemCompare (map IDecl decls ++ map IComment filteredComments)

  ||| Pretty-print a full Module with comments.
  export
  prettyModule : {opts : _} -> Module -> List C.Comment -> Doc opts
  prettyModule mod comments =
    let header = keyword "module" <++> line (show (IS.Module.moduleNS mod))
        imps = vsep (map prettyImport (IS.Module.imports mod))
        items = mergeItems (IS.Module.decls mod) comments
        declDocs = interleaveItems items
        decls = vsep declDocs
    in case (IS.Module.imports mod) of
         [] => (header `vappend` line "") `vappend` decls
         _  => (header `vappend` imps) `vappend` (line "" `vappend` decls)
