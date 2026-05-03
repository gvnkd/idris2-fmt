module IdrisFmt.Parser

import Data.List as L

import public Parser.Source as PS
import public Parser.Rule.Source as PRS
import public Core.Core as CC
import public Core.FC as CFC
import public Core.Name as CN
import public Idris.Syntax as IS
import public Idris.Parser as IP
import public TTImp.TTImp as TT

import IdrisFmt.AST as AST
import IdrisFmt.Comments as C

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
  translatePTerm (PHole _ _ s) = AST.EHole s
  translatePTerm (PDelayed _ lr x) = AST.EDelayed (translatePTerm x)
  translatePTerm (PDelay _ x) = AST.EDelay (translatePTerm x)
  translatePTerm (PForce _ x) = AST.EForce (translatePTerm x)
  translatePTerm (PBracketed _ x) = AST.EBracketed (translatePTerm x)
  translatePTerm (PDotted _ x) = AST.EDotted (translatePTerm x)
  translatePTerm (PAs _ _ n pat) = AST.EAs (translateName n) (translatePTerm pat)
  translatePTerm (POp _ lhsInfo op rhs) =
    AST.EOp (translatePTerm lhsInfo.val.getLhs) (translateOpStr op.val) (translatePTerm rhs)
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
        IS.PData doc vis treq dataDecl =>
          AST.DComment (MkComment LineComment "PData not yet translated" 0 0)
        IS.PParameters params decls =>
          AST.DComment (MkComment LineComment "PParameters not yet translated" 0 0)
        IS.PUsing usings decls =>
          AST.DComment (MkComment LineComment "PUsing not yet translated" 0 0)
        IS.PInterface vis constraints name doc params det conName methods =>
          AST.DComment (MkComment LineComment "PInterface not yet translated" 0 0)
        IS.PImplementation vis opts pass implicits constraints name params implName nusing body =>
          AST.DComment (MkComment LineComment "PImplementation not yet translated" 0 0)
        IS.PRecord doc vis treq recDecl =>
          AST.DComment (MkComment LineComment "PRecord not yet translated" 0 0)
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

||| Parse a full module from source text.
||| Uses Idris2's built-in parser.
export
parseModule : String -> Either ParseError (List (AST.Decl AST.Name))
parseModule src =
  let origin = CFC.Virtual CFC.Interactive
      result = PS.runParser origin Nothing src (IP.prog origin)
   in case result of
        Left err => Left (fromError err)
        Right (_, (_, mod)) => Right (map translatePDecl (IS.Module.decls mod))

||| Parse a single expression from source text.
export
parseExpr : String -> Either ParseError (AST.Expr AST.Name)
parseExpr src = ?rhs_parseExpr
