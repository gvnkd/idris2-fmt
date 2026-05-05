#!/usr/bin/env python3
"""
Mechanically convert Printer.idr from pure to monadic style.
This is a best-effort conversion that handles the common cases.
"""

import re

# Read the original file
with open('src/IdrisFmt/Printer.idr', 'r') as f:
    content = f.read()

# 1. Update imports
content = content.replace(
    'import Text.PrettyPrint.Bernardy.Interface',
    'import Text.PrettyPrint.Bernardy.Interface\nimport IdrisFmt.Monad as M\nimport Control.Monad.RWS'
)

# 2. Convert forward declarations - this is tricky, let's do specific ones
declarations = [
    ('binderDoc : {opts : _} -> AST.RigCount -> AST.Expr\n                                            AST.Name -> AST.Expr\n                                                          AST.Name -> Doc opts',
     'binderDoc : {opts : _} -> AST.RigCount -> AST.Expr\n                                            AST.Name -> AST.Expr\n                                                          AST.Name -> M.PrinterM (Doc opts)'),
    ('implNameDoc : {opts : _} -> Maybe AST.Name -> Doc opts',
     'implNameDoc : {opts : _} -> Maybe AST.Name -> M.PrinterM (Doc opts)'),
    ('fnOptDoc : {opts : _} -> AST.FnOpt -> Doc opts',
     'fnOptDoc : {opts : _} -> AST.FnOpt -> M.PrinterM (Doc opts)'),
    ('paramDoc : {opts : _} -> (AST.Name, Maybe (AST.Expr AST.Name)) -> Doc opts',
     'paramDoc : {opts : _} -> (AST.Name, Maybe (AST.Expr AST.Name)) -> M.PrinterM (Doc opts)'),
    ('usingDoc : {opts : _} -> (Maybe AST.Name, AST.Expr AST.Name) -> Doc opts',
     'usingDoc : {opts : _} -> (Maybe AST.Name, AST.Expr AST.Name) -> M.PrinterM (Doc opts)'),
    ('conNameDoc : {opts : _} -> AST.Name -> Doc opts',
     'conNameDoc : {opts : _} -> AST.Name -> M.PrinterM (Doc opts)'),
    ('branchDoc : {opts : _} -> Doc opts -> AST.Expr AST.Name -> Doc opts',
     'branchDoc : {opts : _} -> M.PrinterM (Doc opts) -> AST.Expr AST.Name -> M.PrinterM (Doc opts)'),
    ('withComments : {opts : _} -> List C.Comment -> Doc opts -> Doc opts',
     'withComments : {opts : _} -> List C.Comment -> M.PrinterM (Doc opts) -> M.PrinterM (Doc opts)'),
    ('visibilityDoc : {opts : _} -> AST.Visibility -> Doc opts',
     'visibilityDoc : {opts : _} -> AST.Visibility -> M.PrinterM (Doc opts)'),
    ('interfaceParamDoc : {opts : _} -> (AST.Name, AST.Expr AST.Name) -> Doc opts',
     'interfaceParamDoc : {opts : _} -> (AST.Name, AST.Expr AST.Name) -> M.PrinterM (Doc opts)'),
    ('importDoc : {opts : _} -> Bool -> List String -> Maybe String -> Doc opts',
     'importDoc : {opts : _} -> Bool -> List String -> Maybe String -> M.PrinterM (Doc opts)'),
    ('implDeclDoc : {opts : _}\n              -> Maybe AST.Name\n              -> AST.Name\n              -> List (AST.Expr AST.Name)\n              -> Maybe (List (AST.Decl AST.Name))\n              -> Doc opts',
     'implDeclDoc : {opts : _}\n              -> Maybe AST.Name\n              -> AST.Name\n              -> List (AST.Expr AST.Name)\n              -> Maybe (List (AST.Decl AST.Name))\n              -> M.PrinterM (Doc opts)'),
]

for old, new in declarations:
    content = content.replace(old, new)

# 3. Convert main pretty function declarations
pretty_decls = [
    ('prettyName : {opts : _} -> Prec -> AST.Name -> Doc opts',
     'prettyName : {opts : _} -> AST.Name -> M.PrinterM (Doc opts)'),
    ('prettyExpr : {opts : _} -> Prec -> AST.Expr AST.Name -> Doc opts',
     'prettyExpr : {opts : _} -> AST.Expr AST.Name -> M.PrinterM (Doc opts)'),
    ('prettyDecl : {opts : _} -> Prec -> AST.Decl AST.Name -> Doc opts',
     'prettyDecl : {opts : _} -> AST.Decl AST.Name -> M.PrinterM (Doc opts)'),
    ('prettyClause : {opts : _} -> Prec -> AST.Clause AST.Name -> Doc opts',
     'prettyClause : {opts : _} -> AST.Clause AST.Name -> M.PrinterM (Doc opts)'),
    ('prettyDoStmt : {opts : _} -> Prec -> AST.DoStmt AST.Name -> Doc opts',
     'prettyDoStmt : {opts : _} -> AST.DoStmt AST.Name -> M.PrinterM (Doc opts)'),
    ('prettyStringPart : {opts : _} -> Prec -> AST.StringPart AST.Name -> Doc opts',
     'prettyStringPart : {opts : _} -> AST.StringPart AST.Name -> M.PrinterM (Doc opts)'),
    ('prettyConDecl : {opts : _} -> Prec -> AST.ConDecl AST.Name -> Doc opts',
     'prettyConDecl : {opts : _} -> AST.ConDecl AST.Name -> M.PrinterM (Doc opts)'),
    ('prettyFieldDecl : {opts : _} -> Prec -> AST.FieldDecl AST.Name -> Doc opts',
     'prettyFieldDecl : {opts : _} -> AST.FieldDecl AST.Name -> M.PrinterM (Doc opts)'),
    ('prettyDataDecl : {opts : _} -> Prec -> AST.DataDecl AST.Name -> Doc opts',
     'prettyDataDecl : {opts : _} -> AST.DataDecl AST.Name -> M.PrinterM (Doc opts)'),
    ('prettyRecordDecl : {opts : _} -> Prec -> AST.RecordDecl AST.Name -> Doc opts',
     'prettyRecordDecl : {opts : _} -> AST.RecordDecl AST.Name -> M.PrinterM (Doc opts)'),
    ('prettyInterfaceDecl : {opts : _} -> Prec -> AST.InterfaceDecl\n                                              AST.Name -> Doc opts',
     'prettyInterfaceDecl : {opts : _} -> AST.InterfaceDecl\n                                              AST.Name -> M.PrinterM (Doc opts)'),
    ('prettyImplDecl : {opts : _} -> Prec -> AST.ImplDecl AST.Name -> Doc opts',
     'prettyImplDecl : {opts : _} -> AST.ImplDecl AST.Name -> M.PrinterM (Doc opts)'),
    ('prettyFixityDecl : {opts : _} -> Prec -> AST.FixityDecl -> Doc opts',
     'prettyFixityDecl : {opts : _} -> AST.FixityDecl -> M.PrinterM (Doc opts)'),
    ('prettyImportDecl : {opts : _} -> Prec -> AST.ImportDecl -> Doc opts',
     'prettyImportDecl : {opts : _} -> AST.ImportDecl -> M.PrinterM (Doc opts)'),
    ('prettyConstant : {opts : _} -> Prec -> AST.Constant -> Doc opts',
     'prettyConstant : {opts : _} -> AST.Constant -> M.PrinterM (Doc opts)'),
    ('prettyOpStr : {opts : _} -> Prec -> AST.OpStr AST.Name -> Doc opts',
     'prettyOpStr : {opts : _} -> AST.OpStr AST.Name -> M.PrinterM (Doc opts)'),
    ('prettyComment : {opts : _} -> Prec -> C.Comment -> Doc opts',
     'prettyComment : {opts : _} -> C.Comment -> M.PrinterM (Doc opts)'),
]

for old, new in pretty_decls:
    content = content.replace(old, new)

# 4. Convert Pretty instances
old_instances = '''mutual
  export
  implementation Pretty AST.Name where
    prettyPrec =
      prettyName
  export
  implementation Pretty (AST.Expr AST.Name) where
    prettyPrec =
      prettyExpr
  export
  implementation Pretty (AST.Decl AST.Name) where
    prettyPrec =
      prettyDecl
  export
  implementation Pretty (AST.Clause AST.Name) where
    prettyPrec =
      prettyClause
  export
  implementation Pretty (AST.DoStmt AST.Name) where
    prettyPrec =
      prettyDoStmt
  export
  implementation Pretty (AST.StringPart AST.Name) where
    prettyPrec =
      prettyStringPart'''

new_instances = '''mutual
  export
  implementation Pretty AST.Name where
    prettyPrec p n =
      fst (M.evalPrinterM defaultConfig p (Opts 80) (prettyName n))
  export
  implementation Pretty (AST.Expr AST.Name) where
    prettyPrec p e =
      fst (M.evalPrinterM defaultConfig p (Opts 80) (prettyExpr e))
  export
  implementation Pretty (AST.Decl AST.Name) where
    prettyPrec p d =
      fst (M.evalPrinterM defaultConfig p (Opts 80) (prettyDecl d))
  export
  implementation Pretty (AST.Clause AST.Name) where
    prettyPrec p c =
      fst (M.evalPrinterM defaultConfig p (Opts 80) (prettyClause c))
  export
  implementation Pretty (AST.DoStmt AST.Name) where
    prettyPrec p s =
      fst (M.evalPrinterM defaultConfig p (Opts 80) (prettyDoStmt s))
  export
  implementation Pretty (AST.StringPart AST.Name) where
    prettyPrec p s =
      fst (M.evalPrinterM defaultConfig p (Opts 80) (prettyStringPart s))'''

content = content.replace(old_instances, new_instances)

# 5. Convert the rest of instances
content = content.replace(
    '''  export
  implementation Pretty (AST.ConDecl AST.Name) where
    prettyPrec =
      prettyConDecl''',
    '''  export
  implementation Pretty (AST.ConDecl AST.Name) where
    prettyPrec p c =
      fst (M.evalPrinterM defaultConfig p (Opts 80) (prettyConDecl c))'''
)

content = content.replace(
    '''  export
  implementation Pretty (AST.FieldDecl AST.Name) where
    prettyPrec =
      prettyFieldDecl''',
    '''  export
  implementation Pretty (AST.FieldDecl AST.Name) where
    prettyPrec p f =
      fst (M.evalPrinterM defaultConfig p (Opts 80) (prettyFieldDecl f))'''
)

content = content.replace(
    '''  export
  implementation Pretty (AST.DataDecl AST.Name) where
    prettyPrec =
      prettyDataDecl''',
    '''  export
  implementation Pretty (AST.DataDecl AST.Name) where
    prettyPrec p d =
      fst (M.evalPrinterM defaultConfig p (Opts 80) (prettyDataDecl d))'''
)

content = content.replace(
    '''  export
  implementation Pretty (AST.RecordDecl AST.Name) where
    prettyPrec =
      prettyRecordDecl''',
    '''  export
  implementation Pretty (AST.RecordDecl AST.Name) where
    prettyPrec p r =
      fst (M.evalPrinterM defaultConfig p (Opts 80) (prettyRecordDecl r))'''
)

content = content.replace(
    '''  export
  implementation Pretty (AST.InterfaceDecl AST.Name) where
    prettyPrec =
      prettyInterfaceDecl''',
    '''  export
  implementation Pretty (AST.InterfaceDecl AST.Name) where
    prettyPrec p i =
      fst (M.evalPrinterM defaultConfig p (Opts 80) (prettyInterfaceDecl i))'''
)

content = content.replace(
    '''  export
  implementation Pretty (AST.ImplDecl AST.Name) where
    prettyPrec =
      prettyImplDecl''',
    '''  export
  implementation Pretty (AST.ImplDecl AST.Name) where
    prettyPrec p i =
      fst (M.evalPrinterM defaultConfig p (Opts 80) (prettyImplDecl i))'''
)

content = content.replace(
    '''  export
  implementation Pretty AST.FixityDecl where
    prettyPrec =
      prettyFixityDecl''',
    '''  export
  implementation Pretty AST.FixityDecl where
    prettyPrec p f =
      fst (M.evalPrinterM defaultConfig p (Opts 80) (prettyFixityDecl f))'''
)

content = content.replace(
    '''  export
  implementation Pretty AST.ImportDecl where
    prettyPrec =
      prettyImportDecl''',
    '''  export
  implementation Pretty AST.ImportDecl where
    prettyPrec p i =
      fst (M.evalPrinterM defaultConfig p (Opts 80) (prettyImportDecl i))'''
)

content = content.replace(
    '''  export
  implementation Pretty AST.Constant where
    prettyPrec =
      prettyConstant''',
    '''  export
  implementation Pretty AST.Constant where
    prettyPrec p c =
      fst (M.evalPrinterM defaultConfig p (Opts 80) (prettyConstant c))'''
)

content = content.replace(
    '''  export
  implementation Pretty (AST.OpStr AST.Name) where
    prettyPrec =
      prettyOpStr''',
    '''  export
  implementation Pretty (AST.OpStr AST.Name) where
    prettyPrec p o =
      fst (M.evalPrinterM defaultConfig p (Opts 80) (prettyOpStr o))'''
)

content = content.replace(
    '''  export
  implementation Pretty C.Comment where
    prettyPrec =
      prettyComment''',
    '''  export
  implementation Pretty C.Comment where
    prettyPrec p c =
      fst (M.evalPrinterM defaultConfig p (Opts 80) (prettyComment c))'''
)

with open('src/IdrisFmt/Printer.idr', 'w') as f:
    f.write(content)

print("Phase 1 complete: declarations and instances converted")
print("Now you need to manually convert function bodies")
