{- ------------------------------------
   Fun: a minimal funcional language
   ------------------------------------

   This module provides a simpler parser from a Haskell-like
   concrete syntax into the Fun abstract syntax.
   Uses the Parsec parser combinator library; for documentation
   see http://legacy.cs.uu.nl/daan/parsec.html

   Pedro Vasconcelos, 2009-2014
   pbv@dcc.fc.up.pt

   Adapted by Pedro Paredes
 -}
module FunParser where
import Fun
import Data.List
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P

-- setup a Haskell-like lexer using the Parsec tokenizer 
lexer = P.makeTokenParser fun
  where fun = haskellStyle { reservedNames = ["ifzero", "then", "else",
                                              "let", "in", "fix",
                                              "cons", "case", "of",
                                              "otherwise"]
                           , reservedOpNames = ["=", "->", "\\", 
                                                "*", "+", "-"],
                             identStart = letter <|> underscore
                           }

reserved   = P.reserved lexer
reservedOp = P.reservedOp lexer
identifier = P.identifier lexer 
natural    = P.natural lexer
parens     = P.parens lexer
brackets   = P.brackets lexer
whiteSpace = P.whiteSpace lexer
symbol     = P.symbol lexer
comma      = symbol ","
bar        = symbol "|"
colon      = symbol ":"
underscore = char '_'


{- This is the start non-terminal: parses a term
   followed by whitespace and end-of-file

   Examples (at the GHCi prompt):
   > parseTest start "let x=1 in 2*(x+1)"
   Let "x" (Const 1) (Const 2 :* (Var "x" :+ Const 1))
   > parseTest start "\\x y->x+y"
   Lambda "x" (Lambda "y" (Var "x" :+ Var "y")
   > parseTest start "x*2)"
   parse error at (line 1, column 4):
   unexpected ")"
   expecting digit, identifier, natural, "(", "*", operator or end of input

   The "parseTest" function  is exported by the Parsec library;
   see also the documentation on "parse" and "parseFromFile".
-}
start :: Parser Term  -- start non-terminal
start = do e<-term
           whiteSpace
           eof 
           return e 

term :: Parser Term      -- top-level expression
term = do reserved "let"  -- let/in expression
          x<-identifier
          reservedOp "="
          e1<-term
          reserved "in"
          e2<-term
          return (Let x e1 e2)
       <|> do reserved "ifzero"   -- ifzero/then/else
              e1 <- term
              reserved "then"
              e2 <- term
              reserved "else"
              e3 <- term
              return (IfZero e1 e2 e3)
       <|> do reserved "fix"         -- fixpoint operator
              e <- term
              return (Fix e)
       -- lambda-abstraction
       -- multiple identifiers are expanded into curried form
       -- i.e.: "\x y z -> e" ==> "\x -> \y -> \z -> e"
       <|> do reservedOp "\\"  
              xs <- many1 identifier  
              reservedOp "->"
              e <- term
              return (foldr Lambda e xs)
       <|> pairTerm
       <|> do reserved "cons"
              ident <- identifier
              e <- parens term
              return (Cons ident e)
       <|> do reserved "case"
              e <- term
              reserved "of"
              xs <- caseTerm `sepBy` bar
              return (CaseS e xs)
       <|> listTerm
       -- otherwise: operator expressions
       <|> opterm

-- parse a pair
pairTerm :: Parser Term
pairTerm = do es <- parens (term `sepBy1` comma)
              return (joinPair es)

-- join elements for pair in pair construct
joinPair :: [Term] -> Term
joinPair [x] = x
joinPair (x:xs) = Pair x (joinPair xs)

-- parse a list
listTerm :: Parser Term
listTerm = do ls <- brackets (term `sepBy` comma)
              return (buildList ls)

-- an expression used in a case construct
caseTerm :: Parser (Name, Ident, Term)
caseTerm = do nm <- identifier
              ident <- parens identifier
              reservedOp "->"
              e <- term
              return (nm, ident, e)
           <|> do reserved "otherwise"
                  reservedOp "->"
                  e <- term
                  return ("", "_", e)

-- builds a list term from an actual list
buildList :: [Term] -> Term
buildList [] = Cons "nil" (Const 0)
buildList (x:xs) = (Cons "list" (Pair x (buildList xs)))

-- an expression using infix arithmetic and comparision operators
-- uses the Parsec expression parser builder
opterm :: Parser Term
opterm = buildExpressionParser table apterm
    where table = [[binary ":" (\x y -> Cons "list" (Pair x y)) AssocRight],
                   [binary "*" (:*) AssocLeft],
                   [binary "+" (:+) AssocLeft, 
                    binary "-" (:-) AssocLeft]]
          binary s op assoc 
              = Infix (do {reservedOp s; return op}) assoc

-- an application (e0 e1 e2 .. en)
-- where ei are self-delimited expressions
apterm :: Parser Term
apterm = do es<-many1 delterm
            return (foldl1 App es)



-- self-delimited expressions: 
-- identifiers, constants or parentesised terms
delterm :: Parser Term
delterm = do x<-identifier 
             return (Var x) 
          <|> do n<-natural 
                 return (Const (fromInteger n))
          <|> try (parens term)
          <|> pairTerm
          <|> listTerm

-- Applies a function to term
applyTo :: (Term -> Term) -> Term -> Term
applyTo f t = case t of
  Lambda x e -> Lambda x (f e)
  App e1 e2 -> App (f e1) (f e2)
  e1 :+ e2 -> (f e1) :+ (f e2)
  e1 :- e2 -> (f e1) :- (f e2)
  e1 :* e2 -> (f e1) :* (f e2)
  IfZero e1 e2 e3 -> IfZero (f e1) (f e2) (f e3)
  Let i e1 e2 -> Let i (f e1) (f e2)
  Fix e -> Fix (f e)
  Pair e1 e2 -> Pair (f e1) (f e2)
  Cons n e -> Cons n (f e)
  Case t xs -> Case (f t) (map (\(x, y) -> (f x, f y)) xs)
  CaseS t xs -> CaseS t (map (\(n, i, e) -> (n, i, (f e))) xs)
  _ -> t

-- Does a post processing job of converting
-- fst and snd functions into the low level
-- implementation
postProcess :: Term -> Term
postProcess t = case t of
  App e1 e2 -> case e1 of
    Var "fst" -> Fst (pp e2)
    Var "snd" -> Snd (pp e2)
    _         -> App (pp e1) (pp e2)
  Case t xs -> App (pp (Lambda
                        (getCaseVar 0)
                        (match (map (\(x, y) -> ([x], y)) xs) 0)))
               (pp t)
  t -> applyTo (pp) t
  where pp = postProcess

-- Get the renamed variables to use in cases
getCaseVar :: Int -> String
getCaseVar x = "-V" ++ (show x)

-- Rename all occurrences of variable 'fr'
-- to 'to' in a term
renameVar :: String -> String -> Term -> Term
renameVar fr to (Var x) = Var y
  where y = if x == fr then to else x
renameVar fr to x = applyTo (renameVar fr to) x

-- Check if term is variable
isVar :: Term -> Bool
isVar (Var x) = True
isVar _ = False

-- Eliminate and propagate variable in variable rule
elemVar :: Int -> ([Term], Term) -> ([Term], Term)
elemVar v (cs, x) = (tail cs, renameVar (getVar $ head cs) (getCaseVar v) x)
  where getVar (Var x) = x

-- Check if term is pair
isPair :: Term -> Bool
isPair (Pair _ _) = True
isPair _ = False

-- Split a pair
dePair :: ([Term], Term) -> ([Term], Term)
dePair (x:xs, t) = ((split x) ++ xs, t)
  where split (Pair a b) = [a, b]

-- Check if term is constructor
isCons :: Term -> Bool
isCons (Cons _ _) = True
isCons _ = False

-- Get the term constructor label
getCons :: Term -> String
getCons (Cons s _) = s

-- Split a constructor
deCons :: ([Term], Term) -> ([Term], Term)
deCons (x:xs, t) = ((split x) ++ xs, t)
  where split (Cons _ a) = [a]

-- Check if term is constant
isConst :: Term -> Bool
isConst (Const _) = True
isConst _ = False

-- Get the value of a constant
getConst :: Term -> Int
getConst (Const a) = a

-- Split a constant
deConst :: ([Term], Term) -> ([Term], Term)
deConst (x:xs, t) = (xs, t)

-- Pattern matching generator (TODO: mixture rule)
match :: [([Term], Term)] -> Int -> Term
match xs v
  | allEmpty xs = snd $ head xs -- The empty rule
  | allVar xs   = match (map (elemVar v) xs) (v + 1) -- The variable rule
  | allConst xs = foldl -- The constant rule
                  (\y (c, x) -> IfZero ((Var (getCaseVar v)) :- Const c) x y)
                  (CaseS (Cons "P" (Const 0)) [("D", "_", Const 0)])
                  (map
                   (\x -> (
                       (getConst . head . fst . head) x,
                       match (map deConst x) (v + 1)))
                   (groupBy
                    (\a b -> let key = (getConst . head . fst) in key a == key b)
                    xs))
  | allPair xs = Let (getCaseVar (v + 1)) -- The pair rule
                  (Fst (Var (getCaseVar v)))
                  (Let (getCaseVar (v + 2))
                   (Snd (Var (getCaseVar v)))
                   (match (map dePair xs) (v + 1)))
  | allCons xs  = CaseS (Var (getCaseVar v)) -- The constructor rule
                  (map
                  (\x -> (
                      (getCons . head . fst . head) x,
                      getCaseVar (v + 1),
                      match (map deCons x) (v + 1)))
                  (groupBy
                   (\a b -> let key = (getCons . head . fst) in key a == key b)
                   xs))
  where allEmpty s = all (null . fst) s
        allVar s   = all (isVar . head . fst) s
        allConst s = all (isConst . head . fst) s
        allPair s  = all (isPair . head . fst) s
        allCons s  = all (isCons . head . fst) s

-- end of file ---
