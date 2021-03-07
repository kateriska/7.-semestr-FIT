
module Types where

import System.Environment
import Data.List.Split
import Data.List
import Data.Tuple

listToTuple :: [[Char]] -> ([Char],[Char])
listToTuple [x,y] = (x,y)

showRule :: ([Char], [Char]) -> [Char]
showRule (x, y) = (x ++ "->" ++ y)




type Rule = (String, [String])

data CFG_t = CFG_t
  {
    nonterminal_symbols :: [String],
    terminal_symbols :: [String],
    starting_symbol :: String,
    grammar_rules :: [([Char], [Char])]
  } deriving (Eq, Read)

--
instance Show CFG_t where
  show (CFG_t nonterminal_symbols terminal_symbols starting_symbol grammar_rules) =
    (intercalate "," nonterminal_symbols) ++ "\n" ++
    (intercalate "," terminal_symbols) ++ "\n" ++
    starting_symbol ++ "\n" ++
    (unlines (map showRule grammar_rules))
