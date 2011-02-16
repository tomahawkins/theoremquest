module Main where

import System.Environment

import LoK

main :: IO ()
main = do
  args <- getArgs
  case args of
    [newLog] -> runLoK Nothing newLog test
    [oldLog, newLog] -> runLoK (Just oldLog) newLog test
    _ -> return ()
    
test :: LoK ()
test = do
  createUser "tomahawkins"
  inference "tomahawkins" $ REFL $ Var (Variable "x" TypeTerm)
  return ()

