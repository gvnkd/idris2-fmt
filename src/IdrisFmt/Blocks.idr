module IdrisFmt.Blocks
import Data.List
import IdrisFmt.AST as AST

%default total

||| A let block: multiple bindings followed by a scope expression.
public export
record LetBlock where
  constructor MkLetBlock
  bindings : List
               (AST.RigCount, (AST.Expr
                                 AST.Name, (AST.Expr
                                              AST.Name, AST.Expr AST.Name)))
  scope : AST.Expr AST.Name

||| Extract a LetBlock from nested ELet nodes.
export flattenELet : AST.Expr AST.Name
                       -> Maybe (LetBlock, List (AST.Clause AST.Name))
flattenELet (ELet rig pat ty val scope alts) =
  case flattenELet scope of
    Nothing =>
      Just (MkLetBlock [(rig, (pat, (ty, val)))] scope, alts)
    Just (MkLetBlock bs sc, _) =>
      Just (MkLetBlock ((rig, (pat, (ty, val))) :: bs) sc, alts)
flattenELet _ =
  Nothing

||| A pi block: multiple parameters followed by a result type.
public export
record PiBlock where
  constructor MkPiBlock
  params : List
             (AST.RigCount, (AST.PiInfo
                               (AST.Expr
                                  AST.Name), (Maybe AST.Name, AST.Expr
                                                                AST.Name)))
  result : AST.Expr AST.Name

||| Extract a PiBlock from nested EPi nodes.
export flattenEPi : AST.Expr AST.Name -> Maybe PiBlock
flattenEPi (EPi rig info n arg ret) =
  case flattenEPi ret of
    Nothing =>
      Just (MkPiBlock [(rig, (info, (n, arg)))] ret)
    Just (MkPiBlock ps res) =>
      Just (MkPiBlock ((rig, (info, (n, arg))) :: ps) res)
flattenEPi _ =
  Nothing
