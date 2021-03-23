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

-- Defined data type of context free grammar:
-- List on nonterminal symbols e.g.
-- List of terminal symbols
-- Starting nonterminal symbol
-- List of tuples of grammar rules
-- e.g. CFG_t {nonterminal_symbols = ["S","A","B"], terminal_symbols = ["a","b"], starting_symbol = "S", grammar_rules = [("S","aAB"),("S","BA"),("A","BBB"),("A","a"),("B","AS"),("B","b")]}

data CFG_t = CFG_t
  {
    nonterminal_symbols :: [String],
    terminal_symbols :: [String],
    starting_symbol :: String,
    grammar_rules :: [([Char], [Char])]
  } deriving (Eq, Read)

-- Instance of how should be type of CFG showed for user
-- e.g.
{-
S,A,B
a,b
S
S->aAB
S->BA
A->BBB
A->a
B->AS
B->b
-}
instance Show CFG_t where
  show (CFG_t nonterminal_symbols terminal_symbols starting_symbol grammar_rules) =
    (intercalate "," nonterminal_symbols) ++ "\n" ++
    (intercalate "," terminal_symbols) ++ "\n" ++
    starting_symbol ++ "\n" ++
    (intercalate "\n" (map showRule grammar_rules))
