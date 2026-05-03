module IdrisFmt.Parser

import Data.List as L

import IdrisFmt.AST as AST
import IdrisFmt.Comments as C

%default total

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

||| Parse a full module from source text.
||| Phase 1: type hole. Will eventually call Idris2 compiler parser
||| and translate PDecl / PTerm to IdrisFmt.AST.
export
parseModule : String -> Either ParseError (List (AST.Decl AST.Name))
parseModule src = Left (ParseErr "Parser not yet implemented in Phase 1")

||| Parse a single expression from source text.
export
parseExpr : String -> Either ParseError (AST.Expr AST.Name)
parseExpr src = Left (ParseErr "Parser not yet implemented in Phase 1")
