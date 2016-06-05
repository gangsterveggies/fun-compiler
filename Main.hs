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
   - Pairs and generic tuples;
   - Lists, variants and records;
   - Case guards (with patter matching);

  Pedro Paredes, 2016
-}

module Main where

import System.Environment
import Text.ParserCombinators.Parsec
import Fun
import FunParser
import SECD2

main :: IO ()
main = do
  args <- getArgs
--  cd <- parseCode (args !! 0)
  let cd = postProcess $ ex !! (read $ head args)
  putStrLn $ show $ cd
  putStrIf (length args > 2) $ show $ (resolveLabels . runCodeGen . compileMain) cd
  putStrIf (length args > 2) $ show $ (assemble . resolveLabels . runCodeGen . compileMain) cd
  writeBytecode (args !! 1) $ (assemble . resolveLabels . runCodeGen . compileMain) cd
  return ()


parseCode :: String -> IO (Term)
parseCode file = do
  txt <- readFile file
  case parse start "" txt of
    Left e  -> error $ show e
    Right r -> return (postProcess r)

putStrIf :: Bool -> String -> IO ()
putStrIf False _ = return()
putStrIf True str = putStrLn str
