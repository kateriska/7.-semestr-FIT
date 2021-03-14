-- Project name: bkg-2-cnf
-- Author: Katerina Fortova
-- Login: xforto00
-- Year: 2020 / 2021

-- Module description: Defined data type of context free grammar

module Types where

import System.Environment
import Data.List.Split
import Data.List
import Data.Tuple

-- Transform list tu tuple
listToTuple :: [[Char]] -> ([Char],[Char])
listToTuple [x,y] = (x,y)

-- Correctly show rule with separator
showRule :: ([Char], [Char]) -> [Char]
showRule (x, y) = (x ++ "->" ++ y)

-- Defined data type of context free grammar
data CFG_t = CFG_t
  {
    nonterminal_symbols :: [String],
    terminal_symbols :: [String],
    starting_symbol :: String,
    grammar_rules :: [([Char], [Char])]
  } deriving (Eq, Read)

-- Instance of how should be type of CFG showed for user 
instance Show CFG_t where
  show (CFG_t nonterminal_symbols terminal_symbols starting_symbol grammar_rules) =
    (intercalate "," nonterminal_symbols) ++ "\n" ++
    (intercalate "," terminal_symbols) ++ "\n" ++
    starting_symbol ++ "\n" ++
    (unlines (map showRule grammar_rules))
