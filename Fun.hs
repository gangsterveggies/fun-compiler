{--------------------------------------
  Fun: a minimal functional language
  -------------------------------------

  Fun is a minimal functional language designed to experiment with
  language interpreters and compilers. This module defines the 
  abstract syntax of the language:
   - the only primitive data are integers with primitives +, - and * 
   - if-zero-then-else conditional;
   - single argument lambdas; currying can be used for 
     multiple arguments;
   - single let-definitions; multiple definitions must be 
     translated to nested lets;
   - single recursive function definitions (for simplicity).

  Pedro Vasconcelos, 2008--2012

  Adapted by Pedro Paredes
-}
module Fun where
import Data.List (union, delete)


-- abstract syntax for language terms
data Term = Var Ident               -- variables
          | Lambda Ident Term       -- abstraction
          | App Term Term           -- application
          | Const Int               -- constants 
          | Term :+ Term            -- arithmetic operators
          | Term :- Term
          | Term :* Term
          | IfZero Term Term Term   -- conditional
          | Let Ident Term Term     -- local definition
          | Fix Term                -- fixed-point operator
          | Pair Term Term          -- pair constructor and destructor
          | Fst Term
          | Snd Term
          | Cons Name Term          -- constructor
          | CaseS Term [(Name, Ident, Term)] -- case
          | Case Term [(Term, Term)]
            deriving (Eq, Show)

-- indentifiers are just strings
type Ident = String
type Name = String

-- some syntactical definitions
-- list of identifiers with free occurrences in a term
fv :: Term -> [Ident]
fv (Var x)      = [x]
fv (Lambda x e) = delete x (fv e)
fv (App e1 e2)  =  (fv e1) `union` (fv e2)
fv (Const n)    = []
fv (e1 :+ e2)   = fv e1 `union` fv e2
fv (e1 :* e2)   = fv e1 `union` fv e2
fv (e1 :- e2)   = fv e1 `union` fv e2
fv (IfZero e1 e2 e3) = fv e1 `union` fv e2 `union` fv e3
fv (Let x e1 e2) = fv e1 `union` delete x (fv e2)
fv (Fix e)       = fv e
fv (Pair e1 e2)  = fv e1 `union` fv e2
fv (Fst e)       = fv e
fv (Snd e)       = fv e
fv (Cons _ e)    = fv e


-- end of file -------------------------------------------------

-- Exemplos feitos pelo Pedro Paredes

ex :: [Term]
ex = [
  -- Testar pair, fst e snd
  Let "x" (Const 3) (Fst (Pair (Const 3 :+ Var "x") (Const 1 :+ Var "x"))),
  Let "x" (Const 3) (Snd (Pair (Const 3 :+ Var "x") (Const 1 :+ Var "x"))),
  -- Testar cons (nota: implementar otherwise)
  CaseS (Cons "P" (Const 3)) [("P", "x", Var "x" :+ Const 1), (("S", "x", Var "x" :+ Const 2))],
  CaseS (Cons "S" (Const 3)) [("P", "x", Var "x" :+ Const 1), (("S", "x", Var "x" :+ Const 2))],
  -- Testar pattern matching
  Case (Const 2) [(Var "x", Var "x" :+ Const 2)],
  Case (Pair (Const 2) (Const 3)) [(Pair (Var "x") (Var "y"), Var "x" :+ Var "y")],
  Case (Cons "D" (Const 2)) [(Cons "P" (Var "x"), Var "x" :+ Const 1), (Cons "D" (Var "x"), Var "x" :+ Const 2)],
  Case (Pair (Const 2) (Cons "nil" (Const 0)))
  [(Pair (Var "x") (Cons "nil" (Const 0)), Var "x"),
   (Pair (Var "x") (Cons "list" (Const 0)), Var "x" :+ Const 1)]
  ]
