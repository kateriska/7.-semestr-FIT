module Algorithm2
  (validCNFrule, filterStep4, validStep4Rule, validStep5Rule,
  transformStep4Rule, checkChangedNonterminalsSubstr, removeChangedNonterminalBrackets,
  removeChangedNonterminalComma, checkChangedNonterminals1, checkChangedNonterminals2,
  checkChangedTerminals, checkChangedNonterminals3, newRulesStep6,transformStep5Rule,
  transformStep6Rule, newCNFRules, allCNFRules, updateNontermsSet, processAlgorithm2,
  processAlgorithms)

  where

import System.Environment
import Data.List.Split
import Data.List
import Data.Tuple

import Parser
import Types
import Algorithm1

validCNFrule :: ([Char], [Char]) -> Bool
validCNFrule rule
  -- A -> BC
 | head (fst rule) `elem` ['A'..'Z'] && length (fst rule) == 1 && all (==True) (map isValid (snd rule)) && length (snd rule) == 2 = True
  -- A -> a
 | head (fst rule) `elem` ['A'..'Z'] && length (fst rule) == 1 && head (snd rule) `elem` ['a'..'z'] && length (snd rule) == 1 = True
 | otherwise = False

-- find rules for step4 (k > 2) which are not originally in cnf form
filterStep4 :: [([Char], [Char])] -> [([Char], [Char])]
filterStep4 rules = filter (\m -> length ( snd m) >  2) (filter (\n -> length  (snd n) >  2) rules)

validStep4Rule :: ([Char], [Char]) -> Bool
validStep4Rule rule
 | length ( snd rule) >  2  = True
 | otherwise = False

validStep5Rule :: ([Char], [Char]) -> Bool
validStep5Rule rule
 | length ( snd rule) ==  2  = True
 | otherwise = False
--transformStep4Rule rule [] counter = [] + [(fst ("A","bbbb"), take 1 (snd ("A","bbbb")) ++ "'<" ++ drop 1 (snd("A","bbbb"))  ++ ">")]
-- iterace od counter = delka bbbb (- 1) (jedno prvni pravidlo uz musi byt na zacatku pridane) (4 - 1 = 3)
-- rule je originalni pravidlo se kterym vstupujeme vzdy ("A", "bbbb")
transformStep4Rule :: ([Char], [Char]) -> [([Char], [Char])] -> Int -> [([Char], [Char])]
transformStep4Rule rule [] counter = [] ++ [(fst rule, take 1 (snd rule) ++ "'<" ++ drop 1 (snd rule)  ++ ">")]
transformStep4Rule rule new_added_rules 2 = new_added_rules
transformStep4Rule rule new_added_rules counter = transformStep4Rule (rule) (new_added_rules ++ [((drop 2 (snd (last new_added_rules))), drop 1 (take 2 (drop 2 (snd (last new_added_rules))))  ++ "'<" ++ (drop 2 (drop 2 (snd (last new_added_rules)))))]) (counter - 1)
--transformStep4Rule rule new_added_rules counter = new_added_rules ++ ((drop 2 (snd (last [("A","b'<bbb>")]))), drop 1 (take 2 (drop 2 (snd (last [("A","b'<bbb>")]))))  ++ "'<" ++ (drop 2 (drop 2 (snd (last [("A","b'<bbb>")])))))

checkChangedNonterminalsSubstr :: [String] -> ([Char], [Char]) -> [Bool]
checkChangedNonterminalsSubstr [x] rule = [(isInfixOf x (snd rule))]
checkChangedNonterminalsSubstr (x:xs) rule = [(isInfixOf x (snd rule))] ++ checkChangedNonterminalsSubstr xs rule

removeChangedNonterminalBrackets :: String -> String
removeChangedNonterminalBrackets xs = [ x | x <- xs, not (x `elem` "<>") ]

removeChangedNonterminalComma :: String -> String
removeChangedNonterminalComma xs = [ x | x <- xs, not (x `elem` "'") ]

-- remove <A> to A
checkChangedNonterminals1 :: [[Char]] -> ([Char], [Char]) -> ([Char], [Char])
checkChangedNonterminals1 nonterminals rule
 | (all (==False) (checkChangedNonterminalsSubstr (map (\n -> "<" ++ n ++ ">") nonterminals) (rule))) = rule
 | otherwise = ((fst rule), removeChangedNonterminalBrackets (snd rule) )

-- remove A' to A
checkChangedNonterminals2 :: [[Char]] -> ([Char], [Char]) -> ([Char], [Char])
checkChangedNonterminals2 nonterminals rule
 | (all (==False) (checkChangedNonterminalsSubstr (map (\n -> n ++ "'") nonterminals) (rule))) = rule
 | otherwise = ((fst rule), removeChangedNonterminalComma(snd rule) )

-- remove <a> to a'
checkChangedTerminals :: [[Char]] -> ([Char], [Char]) -> ([Char], [Char])
checkChangedTerminals terminals rule
 | (all (==False) (checkChangedNonterminalsSubstr (map (\n -> "<" ++ n ++ ">") terminals) (rule))) = rule
 | otherwise = ((fst rule), removeChangedNonterminalBrackets (snd rule) ++ "'" )

-- A -> a'B' to A -> a'B
-- A -> B'b' to A -> Bb'
-- A -> b'B' to A -> b'B
checkChangedNonterminals3 :: [String] -> ([Char], [Char]) -> ([Char], [Char])
checkChangedNonterminals3 nonterminals rule
 | take 2 (snd rule) `elem` (map (\n -> n ++ "'") nonterminals)  = (fst rule, take 1 (snd rule)  ++ drop 2 (snd rule))
 | drop 2 (snd rule) `elem` (map (\n -> n ++ "'") nonterminals)  = (fst rule, take 3 (snd rule))
 | otherwise = rule

newRulesStep6 :: [[Char]] -> ([Char], [Char]) -> [([Char], [Char])]
newRulesStep6 terminals rule
 | take 2 (snd rule) `elem` (map (\n -> n ++ "'") terminals) && drop 2 (snd rule) `elem` (map (\n -> n ++ "'") terminals) = [(take 2 (snd rule), take 1 (snd rule)), (drop 2 (snd rule), take 1 (drop 2 (snd rule)))]
 | take 2 (snd rule) `elem` (map (\n -> n ++ "'") terminals) && (drop 2 (snd rule) `elem` (map (\n -> n ++ "'") terminals)) == False = [(take 2 (snd rule), take 1 (snd rule))]
 | (take 2 (snd rule) `elem` (map (\n -> n ++ "'") terminals)) == False && drop 2 (snd rule) `elem` (map (\n -> n ++ "'") terminals) = [(drop 2 (snd rule), take 1 (drop 2 (snd rule)))]
  -- specially for <Td>->Td'
 | drop 1 (snd rule) `elem` (map (\n -> n ++ "'") terminals) = [(drop 1 (snd rule), take 1 (drop 1 (snd rule)))]
 | otherwise = []

transformStep5Rule :: ([Char], [Char]) -> ([Char], [Char])
transformStep5Rule rule = ((fst rule), take 1 (snd (rule)) ++ "'" ++ drop 1 (snd (rule)) ++  "'" )

transformStep6Rule :: [([Char], [Char])] -> [String] -> [([Char], [Char])]
transformStep6Rule [x] terminals = newRulesStep6 terminals x
transformStep6Rule (x:xs) terminals = newRulesStep6 terminals x ++ transformStep6Rule xs terminals

newCNFRules ::  [([Char], [Char])] -> [String] -> [String] -> [([Char], [Char])]
newCNFRules [x] nonterminals terminals
 | validCNFrule x == True = [x]
 | validCNFrule x == False && validStep4Rule x == True = map (checkChangedTerminals terminals) (map (checkChangedNonterminals2 nonterminals) (map (checkChangedNonterminals1 nonterminals) (transformStep4Rule x ([] ++ [(fst x, take 1 (snd x) ++ "'<" ++ drop 1 (snd x)  ++ ">")]) (length (snd x)))))
 | validCNFrule x == False && validStep5Rule x == True = [checkChangedNonterminals3 (nonterminals) (transformStep5Rule x)]
 | otherwise = []
newCNFRules (x:xs) nonterminals terminals
 | validCNFrule x == True = [x] ++ newCNFRules xs nonterminals terminals
 | validCNFrule x == False && validStep4Rule x == True =  map (checkChangedTerminals terminals) (map (checkChangedNonterminals2 nonterminals) (map (checkChangedNonterminals1 nonterminals) (transformStep4Rule x ([] ++ [(fst x, take 1 (snd x) ++ "'<" ++ drop 1 (snd x)  ++ ">")]) (length (snd x))))) ++ newCNFRules xs nonterminals terminals
 | validCNFrule x == False && validStep5Rule x == True = [checkChangedNonterminals3 (nonterminals) (transformStep5Rule x)] ++ newCNFRules xs nonterminals terminals
 | otherwise = newCNFRules xs nonterminals terminals
-- | validCNFrule == False && validStep4Rule == True =

-- final rules with Step 6 a' -> a rules
allCNFRules :: [([Char], [Char])] -> [String] -> [String] -> [([Char], [Char])]
allCNFRules rules nonterminals terminals = nub (newCNFRules rules nonterminals terminals ++ transformStep6Rule (newCNFRules rules nonterminals terminals) (terminals))

updateNontermsSet :: [([Char], [Char])] -> [String] -> [String] -> [[Char]]
updateNontermsSet rules nonterminals terminals = nub (map fst (allCNFRules rules nonterminals terminals) ++ nonterminals)

processAlgorithm2 :: CFG_t -> CFG_t
processAlgorithm2 (CFG_t nonterminal_symbols terminal_symbols starting_symbol grammar_rules) = CFG_t (updateNontermsSet grammar_rules nonterminal_symbols terminal_symbols) terminal_symbols starting_symbol (allCNFRules grammar_rules nonterminal_symbols terminal_symbols)

processAlgorithms :: String -> CFG_t -> CFG_t
processAlgorithms "-1" grammar_input = processAlgorithm1 grammar_input
processAlgorithms "-2" grammar_input = processAlgorithm2 (processAlgorithm1 grammar_input)
processAlgorithms switch grammar_input = grammar_input
