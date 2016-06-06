{-# LANGUAGE DeriveFunctor, FlexibleInstances #-}
{- -----------------------------------
   Fun: a minimal functional language
   -----------------------------------
   A byte-code compiler for a SECD-like virtual machine.
   
   Pedro Vasconcelos, 2008--2013.

   Adapted by Pedro Paredes
 -}
module SECD2 where
import Fun
import Data.List (elemIndex)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State
import Data.Hashable (hash)

-----------------------------------------------------------------
-- SECD machine definitions
-----------------------------------------------------------------

-- pseudo instructions parameterized by label type
data Instr l = HALT             -- finished
             | LDC Int          -- load constant
             | LD Int           -- load variable
             | ADD              -- addition
             | SUB              -- subtraction
             | MUL              -- multiplication
             | SEL l l          -- select zero/non-zero
             | LDF l            -- load a closure
             | LDRF l           -- load a recursive closure
             | AP               -- apply
             | RTN              -- return 
             | JOIN             -- close branch
             | PAIR             -- pair constructor
             | FST              -- first from pair
             | SND              -- second from pair
             | CONS Int         -- constructor
             | MATCH [(Int, l)] -- constructor selector
             | REC [(Int, l)]   -- record
             | SLT Int          -- record selector
             | LST              -- less than
             | GTT              -- greater than
             | ROUT             -- output is record
             deriving (Show, Functor)

-- symbolic labels are just strings
type Label = String


-- State monad generating fresh labels and storing code blocks
type CodeGen = State (Map Label [Instr Label])

-- add a new code block segment
-- returns a fresh label
newBlock :: [Instr Label] -> CodeGen Label
newBlock c = do labels <- get
                let l = "L" ++ show (1+Map.size labels) 
                put (Map.insert l c labels)
                return l

-- compile a lambda term into SECD code
compile :: Term -> [Ident] -> CodeGen [Instr Label]

compile (Var x) sym 
    = case elemIndex x sym of
        Nothing -> error ("free variable: " ++ show x)
        Just k -> return [LD k]
-- "elemIndex x xs" 
-- gives the index of first occurence of x in xs or Nothing 

compile (Rout e) sym
  = do code <- compile e sym  
       return (code ++ [ROUT])

compile (Lambda x e) sym 
  = do code <- compile e (x:sym) 
       l <- newBlock (code++[RTN])
       return [LDF l]

-- compile a recursive function
compile (Fix (Lambda f (Lambda x e1))) sym
  = do code <- compile e1 (x:f:sym) 
       l <- newBlock (code++[RTN])
       return [LDRF l]

compile (App e1 e2) sym 
  = do code1 <- compile e1 sym 
       code2 <- compile e2 sym 
       return (code1 ++ code2 ++ [AP])
       
compile (Const n) sym = return [LDC n]

compile (e1 :+ e2) sym 
  = do code1<-compile e1 sym 
       code2<-compile e2 sym 
       return (code1 ++ code2 ++ [ADD])

compile (e1 :- e2) sym 
  = do code1<-compile e1 sym
       code2<-compile e2 sym 
       return (code1 ++ code2 ++ [SUB])

compile (e1 :* e2) sym 
  = do code1<-compile e1 sym 
       code2<-compile e2 sym 
       return (code1 ++ code2 ++ [MUL])

compile (e1 :< e2) sym 
  = do code1<-compile e1 sym 
       code2<-compile e2 sym 
       return (code1 ++ code2 ++ [LST])

compile (e1 :> e2) sym 
  = do code1<-compile e1 sym 
       code2<-compile e2 sym 
       return (code1 ++ code2 ++ [GTT])

compile (IfZero e1 e2 e3) sym
  = do code1 <- compile e1 sym  
       code2 <- compile e2 sym
       code3 <- compile e3 sym
       ltrue <- newBlock (code2 ++ [JOIN])
       lfalse<- newBlock (code3 ++ [JOIN])
       return (code1 ++ [SEL ltrue lfalse])

compile (Let x e1 e2) sym
    = compile (App (Lambda x e2) e1) sym

compile (Pair e1 e2) sym
  = do code1 <- compile e1 sym  
       code2 <- compile e2 sym
       return (code1 ++ code2 ++ [PAIR])

compile (Fst e) sym
  = do code <- compile e sym  
       return (code ++ [FST])

compile (Snd e) sym
  = do code <- compile e sym  
       return (code ++ [SND])

compile (Cons lb e) sym
  = do code <- compile e sym
       return (code ++ [CONS (hashName lb)])

compile (CaseS e alts) sym
  = do code <- compile e sym
       cases <- compileCase alts sym
       return (code ++ [MATCH cases])

compile (Record alts) sym
  = do cases <- compileRecord alts sym
       return ([REC cases])

compile (Select lb e) sym
  = do code <- compile e sym
       return (code ++ [SLT (hashName lb)])

compileCase :: [(Name, Ident, Term)] -> [Ident] -> CodeGen [(Int, Label)]
compileCase [] _ = return []
compileCase ((nm, idt, e):xs) sym
  = do code <- compile e (idt:sym)
       l <- newBlock (code ++ [JOIN])
       cases <- compileCase xs sym
       return ((hashName nm, l) : cases)

compileRecord :: [(Name, Term)] -> [Ident] -> CodeGen [(Int, Label)]
compileRecord [] _ = return []
compileRecord ((nm, e):xs) sym
  = do code <- compile e sym
       l <- newBlock (code ++ [JOIN])
       cases <- compileRecord xs sym
       return ((hashName nm, l) : cases)
       

-- compile the main expression
compileMain :: Term -> CodeGen [Instr Label]
compileMain e = do code <- compile e [] 
                   return (code ++ [HALT])

-- run the code generator
-- code begins at label "l0" which should be <= all other labels
runCodeGen :: CodeGen [Instr Label] -> Map Label [Instr Label]
runCodeGen cgen =  Map.insert "L0" c0 labels  -- code start
  where (c0, labels) = runState cgen Map.empty

-- hash name
hashName :: String -> Int
hashName "" = -1
hashName lb = (hash lb `mod` (2^31 - 1))

-----------------------------------------------------------------------------
-- code addresses are simple integers
type Addr = Int

resolveLabels :: Map Label [Instr Label] -> [Instr Addr]
resolveLabels labels = map resolve $ concat (Map.elems labels)
  where table = symbolTable labels
        resolve = fmap (\l -> Map.findWithDefault undefined l table)

symbolTable :: Map Label [Instr Label] -> Map Label Addr
symbolTable labels = Map.fromList (zip (Map.keys labels) addrs)
  where sizes = map (\c -> sum [sizeof i | i<-c]) (Map.elems labels)
        addrs = scanl (+) 0 sizes

----------------------------------------------------------------------------
-- assemblying into bytecodes
----------------------------------------------------------------------------
type Bytecode = Int -- bytecodes are plain integers

class Asm a where
  assemble :: a -> [Bytecode]
  
instance Asm (Instr Addr) where
  assemble HALT        = [0]
  assemble (LDC n)     = [1, n]
  assemble (LD n)      = [2, n]
  assemble ADD         = [3]
  assemble SUB         = [4]
  assemble MUL         = [5]
  assemble (SEL l1 l2) = [6, l1, l2]
  assemble (LDF l)     = [7, l]
  assemble (LDRF l)    = [8, l]
  assemble AP          = [9]
  assemble RTN         = [10]
  assemble JOIN        = [11]
  assemble PAIR        = [12]
  assemble FST         = [13]
  assemble SND         = [14]
  assemble (CONS n)    = [15, n]
  assemble (MATCH ls)  = [16, length ls] ++ (assembleMatch ls)
    where assembleMatch []          = []
          assembleMatch ((n, l):xs) = [n, l] ++ (assembleMatch xs)
  assemble (REC ls)    = [17, length ls] ++ (assembleRecord ls)
    where assembleRecord []          = []
          assembleRecord ((n, l):xs) = [n, l] ++ (assembleRecord xs)
  assemble (SLT n)     = [18, n]
  assemble LST         = [19]
  assemble GTT         = [20]
  assemble ROUT        = [21]

instance Asm a => Asm [a] where
  assemble = concatMap assemble

sizeof :: Instr l -> Int
sizeof = length . assemble . fmap (\_ -> 0 :: Addr)

writeBytecode :: FilePath -> [Bytecode] -> IO ()
writeBytecode file code = writeFile file (unlines $ map show code)
