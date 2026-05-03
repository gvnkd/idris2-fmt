module IdrisFmt.Doc

import public Text.PrettyPrint.Bernardy as PP
import IdrisFmt.Config as CFG

%default total

||| Convert formatter config to prettier LayoutOpts.
export
toLayoutOpts : CFG.Config -> LayoutOpts
toLayoutOpts (MkConfig _ ll) = Opts ll

||| Render a document to string using the layout options derived from config.
export
renderDoc : (cfg : CFG.Config) -> Doc (toLayoutOpts cfg) -> String
renderDoc cfg doc = Text.PrettyPrint.Bernardy.Core.Doc.render (toLayoutOpts cfg) doc

||| Format a keyword.
export
keyword : {opts : _} -> String -> Doc opts
keyword s = line s

||| Format an operator symbol.
export
operator_ : {opts : _} -> String -> Doc opts
operator_ s = line s

||| Format an identifier.
export
ident : {opts : _} -> String -> Doc opts
ident s = line s

||| Format a string literal.
export
stringLit : {opts : _} -> String -> Doc opts
stringLit s = dquotes (text s)

||| Format a character literal.
export
charLit : {opts : _} -> Char -> Doc opts
charLit c = squotes (line (show c))
