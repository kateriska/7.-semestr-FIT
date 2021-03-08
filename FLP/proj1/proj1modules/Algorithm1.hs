module Algorithm1
  (recursionNA, createNA, checkSimpleRule, isNontermForNA,
  changeLeftSideRule, filterRules, removeSimpleRules, iterateList, processAlgorithm1)

  where

import System.Environment
import Data.List.Split
import Data.List
import Data.Tuple

import Types
import Parser

recursionNA :: [([Char], [Char])] -> Int -> [[Char]] -> [[Char]] -> [[Char]]
--recursionNA rules input_set nonterminals = if
--recursionNA rules input_set nonterminals = do let value1 = recursionNA (rules) (concat (map (createNA rules input_set) nonterminals)) (concat (map (createNA rules input_set) nonterminals))
--                                              let value2 = recursionNA (rules) (value1) (value2)

--                                              if value1 == value2
--                                                then value2
-- init counter je pocet vsech pravidel, snizovani az na nulu a pak konec
recursionNA rules 0 input_set nonterminals = input_set
recursionNA rules counter input_set nonterminals = recursionNA (rules) (counter - 1) (nub(concat (map (createNA rules input_set) nonterminals))) (nub(concat (map (createNA rules input_set) nonterminals)))
--recursionNA rules input_set nonterminals = nub (concat (map (createNA rules input_set) nonterminals))
--recursionNA rules input_set nonterminals = concat (map (createNA rules []) nonterminals)


createNA :: [([Char], [Char])] -> [[Char]] -> [Char] -> [[Char]]
--createNA rules [] nonterminal  = (concat (filter (/= []) (map (isNontermForNA []) (filter ((==nonterminal).fst) rules ))))
createNA rules input_set nonterminal  = (concat (filter (/= []) (map (isNontermForNA input_set) (filter ((==nonterminal).fst) rules ))))



--createNA :: Char -> [([Char], [Char])] -> [[Char]] -> [[Char]]
--createNA nonterminal rules input_set = map checkSimpleRule(filter ((==nonterminal).fst) rules)

checkSimpleRule :: ([Char], [Char]) -> Bool
checkSimpleRule rule
 | isValidStartingSymbol (snd rule) == True = True
 | otherwise = False

--checkSimpleRules :: [([Char], [Char])] -> [Bool]
--checkSimpleRules rules = map checkSimpleRule rules

--addToNA :: [([Char], [Char])] -> [Bool] -> [[Char]] -> [[Char]]
--addToNA rules simple_rules_bool input_set = map addNonterminalToNA


--addNonterminalToNA :: ([Char], [Char]) -> Bool -> [[Char]] -> [[Char]]
--addToNA rule is_simple_rule input_set
--  | is_simple_rule == True = input_set ++ [(snd rule)]
--  | otherwise = input_set

isNontermForNA :: [[Char]] -> ([Char], [Char]) -> [[Char]]
--isNontermForNA [] rule
--  | checkSimpleRule rule == True = [(snd rule)]
--  | otherwise = []
isNontermForNA input_set rule
 | checkSimpleRule rule == True = input_set ++ [(snd rule)]
 | otherwise = input_set

--removeSimpleRules(CFG_t nonterminal_symbols terminal_symbols starting_symbol grammar_rules) = map (recursionNA (rules) (length grammar_rules) ) nonterminal_symbols nonterminal_symbols
--filterRules x = filter ((==x).fst)  rules
changeLeftSideRule :: [Char] -> ([Char], [Char]) -> ([Char], [Char])
changeLeftSideRule nonterminal (x,y) = (nonterminal, y)
--concat (map filterRules ["E","F"])

filterRules :: [([Char], [Char])] -> [Char] -> [([Char], [Char])]
filterRules rules nonterminal = filter ((==nonterminal).fst)  rules

removeSimpleRules :: [Char] -> [([Char], [Char])] -> [[Char]] -> [([Char], [Char])]
removeSimpleRules nonterminal rules na_set = map (changeLeftSideRule nonterminal) (filter (\n ->  checkSimpleRule n == False) (concat (map (filterRules rules) na_set)) )

--accumulateTransformedRules new_rules [] = new_rules
--accumulateTransformedRules new_rules final_set = new_rules ++ final_set
iterateList :: [String] -> [([Char], [Char])] -> [([Char], [Char])]
iterateList [x] rules = removeSimpleRules x rules (recursionNA (rules) (length (rules)) [x] [x])
iterateList (x:xs) rules = removeSimpleRules x rules (recursionNA (rules) (length (rules)) [x] [x]) ++ iterateList xs rules


processAlgorithm1 :: CFG_t -> CFG_t
processAlgorithm1 (CFG_t nonterminal_symbols terminal_symbols starting_symbol grammar_rules) = CFG_t nonterminal_symbols terminal_symbols starting_symbol (iterateList (nonterminal_symbols) (grammar_rules ) )
