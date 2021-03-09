-- Project name: bkg-2-cnf
-- Author: Katerina Fortova
-- Login: xforto00
-- Year: 2020 / 2021

-- Module description: Implementation of Algorithm 4.5 - Remove of simple rules

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

-- Recursively add new nonterms to NA set
recursionNA :: [([Char], [Char])] -> Int -> [[Char]] -> [[Char]] -> [[Char]]
-- init counter is count of all rules, decrement to zero and then end
recursionNA rules 0 input_set nonterminals = input_set
recursionNA rules counter input_set nonterminals = recursionNA (rules) (counter - 1) (nub(concat (map (createNA rules input_set) nonterminals))) (nub(concat (map (createNA rules input_set) nonterminals)))

-- Create NA set
createNA :: [([Char], [Char])] -> [[Char]] -> [Char] -> [[Char]]
--createNA rules [] nonterminal  = (concat (filter (/= []) (map (isNontermForNA []) (filter ((==nonterminal).fst) rules ))))
createNA rules input_set nonterminal  = (concat (filter (/= []) (map (isNontermForNA input_set) (filter ((==nonterminal).fst) rules ))))

-- Check whether rule is simple (A -> B)
checkSimpleRule :: ([Char], [Char]) -> Bool
checkSimpleRule rule
 | isValidStartingSymbol (snd rule) == True = True
 | otherwise = False

-- Add nonterm to NA if the rule is simple
isNontermForNA :: [[Char]] -> ([Char], [Char]) -> [[Char]]
isNontermForNA input_set rule
 | checkSimpleRule rule == True = input_set ++ [(snd rule)]
 | otherwise = input_set

-- Change left side of rule
changeLeftSideRule :: [Char] -> ([Char], [Char]) -> ([Char], [Char])
changeLeftSideRule nonterminal (x,y) = (nonterminal, y)

-- Filter rules with chosen nonterminal in the left side
filterRules :: [([Char], [Char])] -> [Char] -> [([Char], [Char])]
filterRules rules nonterminal = filter ((==nonterminal).fst)  rules

-- Remove all simple rules from rule set
removeSimpleRules :: [Char] -> [([Char], [Char])] -> [[Char]] -> [([Char], [Char])]
removeSimpleRules nonterminal rules na_set = map (changeLeftSideRule nonterminal) (filter (\n ->  checkSimpleRule n == False) (concat (map (filterRules rules) na_set)) )

-- Loop through list of all nonterms and their rules - compute NA sets for all nonterms
iterateList :: [String] -> [([Char], [Char])] -> [([Char], [Char])]
iterateList [x] rules = removeSimpleRules x rules (recursionNA (rules) (length (rules)) [x] [x])
iterateList (x:xs) rules = removeSimpleRules x rules (recursionNA (rules) (length (rules)) [x] [x]) ++ iterateList xs rules

-- Process final Algorithm 4.5 and change the data structure of context free grammar 
processAlgorithm1 :: CFG_t -> CFG_t
processAlgorithm1 (CFG_t nonterminal_symbols terminal_symbols starting_symbol grammar_rules) = CFG_t nonterminal_symbols terminal_symbols starting_symbol (iterateList (nonterminal_symbols) (grammar_rules ) )
