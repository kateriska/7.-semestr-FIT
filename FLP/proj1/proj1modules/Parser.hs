-- Project name: bkg-2-cnf
-- Author: Katerina Fortova
-- Login: xforto00
-- Year: 2020 / 2021

-- Module description: Syntactic analyse of the input of program

module Parser
  (checkArguments, readGrammarInput, isValid, isValidNonterminal,
  isValidTerminal, isValidStartingSymbol, isValidLeftRule, isValidAlphaCharacter,
  isValidRightRule, isValidGrammarRule, startingSymbolInNonterminals,
  parseCFG, checkSyntaxCFG, checkRulesSeparators)

  where
import System.Environment
import Data.List.Split
import Data.List
import Data.Tuple

import Types

-- Decide which argument use and get name of file or figure out STDIN input
checkArguments :: [String] -> (String, String)
checkArguments [switch] = checkArguments [switch, ""]
checkArguments [] = error "Error - Cant parse no arguments"
checkArguments [switch, file]
 | switch == "-i" = ("-i", file)
 | switch == "-1" = ("-1", file)
 | switch == "-2" = ("-2", file)
 | otherwise = error "Error - Wrong switch!"
checkArguments _ = error "Error - Wrong number of arguments!"

-- Read contents of file or STDIN
readGrammarInput :: String -> IO String
readGrammarInput [] = getContents
readGrammarInput file = readFile file

-- Check whether char is uppercase alphabetic
isValid :: Char -> Bool
isValid x
 | x `elem` ['A'..'Z'] = True
 | otherwise = False

-- Check whether nonterminal is one letter of uppercase alphabetic
isValidNonterminal :: [String] -> Bool
isValidNonterminal x
 | (all (`elem` ['A'..'Z']) (map head x) && all (== 1) (map length x)) = True
 | otherwise = False

-- Check whether terminal is one letter of lowercase alphabetic
isValidTerminal :: [String] -> Bool
isValidTerminal x
 | (all (`elem` ['a'..'z']) (map head x) && all (== 1) (map length x)) = True
 | otherwise = False

-- Check whether starting symbol is nonterminal of one letter uppercase alphabetic
isValidStartingSymbol :: String -> Bool
isValidStartingSymbol x
 | length x == 1 && all (`elem` ['A'..'Z']) x = True
 | otherwise = False

-- Check whether left sides of rules have only defined nonterminals from subset of [A..Z]
isValidLeftRule :: [[Char]] -> [[Char]] -> Bool
isValidLeftRule left_rule nonterminal_symbols
 | all (`elem` ['A'..'Z']) (map head left_rule) && all (== 1) (map length left_rule) && all (`elem` (concat (map (take 1) nonterminal_symbols))) (map head left_rule) = True
 | otherwise = False

-- Check whether terminals and nonterminals in rules are in defined terminals and nonterminals
isValidAlphaCharacter :: [[Char]] -> [[Char]] -> Char -> Bool
isValidAlphaCharacter nonterminal_symbols terminal_symbols x
 | x `elem` ['a'..'z'] && x `elem` concat (map (take 1) terminal_symbols) = True
 | x `elem` ['A'..'Z'] && x `elem` concat (map (take 1) nonterminal_symbols) = True
 | otherwise  = False

isValidRightRule :: [[Char]] -> [[Char]] -> [[Char]] -> Bool
isValidRightRule right_rule nonterminal_symbols terminal_symbols
 | all (== True) (map (all (isValidAlphaCharacter nonterminal_symbols terminal_symbols)) right_rule) = True
 | otherwise = False

-- Check correct format of left side and right side of grammar rule
isValidGrammarRule :: [[Char]] -> [[Char]] -> [[Char]] -> [[Char]] -> Bool
isValidGrammarRule left_rule right_rule nonterminal_symbols terminal_symbols
 | isValidLeftRule left_rule nonterminal_symbols && isValidRightRule right_rule nonterminal_symbols terminal_symbols = True
 | otherwise = False

-- Check whether starting symbol is also part of set of nonterminals
startingSymbolInNonterminals :: [[Char]] -> [Char] -> Bool
startingSymbolInNonterminals nonterminal_symbols starting_symbol
 | head starting_symbol `elem` concat (map (take 1) nonterminal_symbols) = True
 | otherwise = False

-- Transform input file or STDIN input to CFG_t structure
parseCFG :: String -> CFG_t
parseCFG grammar_input = CFG_t {
 nonterminal_symbols = splitOn "," (lines (grammar_input) !! 0),
 terminal_symbols = splitOn "," (lines (grammar_input) !! 1),
 starting_symbol = lines (grammar_input) !! 2,
 grammar_rules = map listToTuple (map (splitOn "->") (drop 3 (lines (grammar_input))))
 }

-- Check all syntax analysis of input context free grammar
checkSyntaxCFG :: CFG_t -> Bool
checkSyntaxCFG (CFG_t nonterminal_symbols terminal_symbols starting_symbol grammar_rules)
 | isValidNonterminal nonterminal_symbols && isValidTerminal terminal_symbols && isValidStartingSymbol starting_symbol && isValidGrammarRule (map fst grammar_rules) (map snd grammar_rules) (nonterminal_symbols) (terminal_symbols) && startingSymbolInNonterminals nonterminal_symbols starting_symbol && uniqueSymbols nonterminal_symbols && uniqueSymbols terminal_symbols && uniqueSymbols grammar_rules = True
 | otherwise = False

-- Check only one occurence of nonterminal, terminal, rule in their lists, because duplicitations mustn't be generally on the output (via forum)
uniqueSymbols :: (Eq a) => [a] -> Bool
uniqueSymbols [] = True
uniqueSymbols (x:xs) = x `notElem` xs && uniqueSymbols xs

 -- Check whether each rule has "->" separator
checkRulesSeparators :: String -> Bool
checkRulesSeparators x = all (==True) (map (isInfixOf "->") (drop 3 (lines (x))))
