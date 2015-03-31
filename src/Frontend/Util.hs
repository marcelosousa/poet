module Frontend.Util where

import Language.SimpleC.AST

type Globals = [Ident]

getGlobalsDecls :: Program -> Globals
getGlobalsDecls (Program (decls,defs)) = foldl (\a decl -> convertDecl decl ++ a) [] decls
  where 
    convertDecl decl = case decl of
      FunctionDecl _ _ _ -> [] 
      GlobalDecl _ (Ident i) _ -> [i]
      GlobalDecl _ (Index (Ident i) _) _ -> [i]
      GlobalDecl _ _ _ -> []


