module Parser
  (checkArguments, readGrammarInput, isValid, isValidNonterminal,
  isValidTerminal, isValidStartingSymbol, isValidLeftRule, isValidAlphaCharacter,
  isValidRightRule, isValidGrammarRule, firstRuleStartingSymbol, startingSymbolInNonterminals,
  parseCFG, checkSyntaxCFG, printSyntaxCFGinfo, checkRulesSeparators)

  where
import System.Environment
import Data.List.Split
import Data.List
import Data.Tuple

import Types

checkArguments :: [String] -> (String, String)
checkArguments [switch] = checkArguments [switch, ""]
checkArguments [] = error "Error - Cant parse no arguments"
checkArguments [switch, file]
 | switch == "-i" = ("-i", file)
 | switch == "-1" = ("-1", file)
 | switch == "-2" = ("-2", file)
 | otherwise = error "Error - Wrong switch!"
checkArguments _ = error "Error - Wrong number of arguments!"

readGrammarInput :: String -> IO String
readGrammarInput [] = getContents
readGrammarInput file = readFile file


{-
isLetter :: Char -> Bool
isLetter c = (('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z'))

checkSyntaxNonterminals :: [String] -> [String]
checkSyntaxNonterminals [x:xs]
  | isLetter x == False = False
  | isLetter x == True = True

checkSyntaxNonterminalsOverall :: [String] -> [String]
checkSyntaxNonterminalsOverall xs
  | checkSyntaxNonterminals xs == False = error "blabla"
  | checkSyntaxNonterminals xs == True  = xs
-}

isValid :: Char -> Bool
isValid x
 | x `elem` ['A'..'Z'] = True
 | otherwise = False

isValidNonterminal :: [String] -> Bool
isValidNonterminal x
 | (all (`elem` ['A'..'Z']) (map head x) && all (== 1) (map length x)) = True
 | otherwise = False

isValidTerminal :: [String] -> Bool
isValidTerminal x
 | (all (`elem` ['a'..'z']) (map head x) && all (== 1) (map length x)) = True
 | otherwise = False

isValidStartingSymbol :: String -> Bool
isValidStartingSymbol x
 | length x == 1 && all (`elem` ['A'..'Z']) (x) = True
 | otherwise = False

-- check whether left sides of rules have only defined nonterminals from subset of [A..Z]
isValidLeftRule :: [[Char]] -> [[Char]] -> Bool
isValidLeftRule left_rule nonterminal_symbols
 | all (`elem` ['A'..'Z']) (map head left_rule) && all (== 1) (map length left_rule) && all (`elem` (concat (map (take 1) nonterminal_symbols))) (map head left_rule)   = True
 | otherwise = False

-- check whether terminals and nonterinals in rules are in defined terminals and nonterminals
isValidAlphaCharacter :: [[Char]] -> [[Char]] -> Char -> Bool
isValidAlphaCharacter nonterminal_symbols terminal_symbols x
 | x `elem` ['a'..'z'] && x `elem` concat (map (take 1) terminal_symbols)  = True
 | x `elem` ['A'..'Z'] && x `elem` concat (map (take 1) nonterminal_symbols) = True
 | otherwise  = False

isValidRightRule :: [[Char]] -> [[Char]] -> [[Char]] -> Bool
isValidRightRule right_rule nonterminal_symbols terminal_symbols
 | all (== True) (map  (all (isValidAlphaCharacter nonterminal_symbols terminal_symbols)) right_rule) = True
 | otherwise = False

isValidGrammarRule :: [[Char]] -> [[Char]] -> [[Char]] -> [[Char]] -> Bool
isValidGrammarRule left_rule right_rule nonterminal_symbols terminal_symbols
 | (isValidLeftRule left_rule nonterminal_symbols == True) && (isValidRightRule right_rule nonterminal_symbols terminal_symbols == True) = True
 | otherwise = False


firstRuleStartingSymbol :: [[Char]] -> [Char] -> Bool
firstRuleStartingSymbol left_rule starting_symbol
 | head left_rule == starting_symbol = True
 | otherwise = False

startingSymbolInNonterminals :: [[Char]] -> [Char] -> Bool
startingSymbolInNonterminals nonterminal_symbols starting_symbol
 | head starting_symbol `elem` concat (map (take 1) nonterminal_symbols) = True
 | otherwise = False

parseCFG :: String -> CFG_t
parseCFG grammar_input = CFG_t {
 nonterminal_symbols = (splitOn "," (lines (grammar_input) !! 0)),
 terminal_symbols = splitOn "," (lines (grammar_input) !! 1),
 starting_symbol = lines (grammar_input) !! 2,
 grammar_rules = map listToTuple (map (splitOn "->") (drop 3 (lines (grammar_input))))
 }

checkSyntaxCFG :: CFG_t -> Bool
checkSyntaxCFG (CFG_t nonterminal_symbols terminal_symbols starting_symbol grammar_rules) =
 if syntaxCorrect == True then True
 else False
 where
   syntaxCorrect = isValidNonterminal nonterminal_symbols && isValidTerminal terminal_symbols && isValidStartingSymbol starting_symbol && isValidGrammarRule (map fst grammar_rules) (map snd grammar_rules) (nonterminal_symbols) (terminal_symbols) && firstRuleStartingSymbol (map fst grammar_rules) starting_symbol && startingSymbolInNonterminals nonterminal_symbols starting_symbol

printSyntaxCFGinfo :: Bool -> String
printSyntaxCFGinfo True = "Info - Format of CFG is correct"
printSyntaxCFGinfo False = error "Error - Wrong format of input CFG!"

 --runCorrectSwitch :: String -> String
 --runCorrectSwitch "-i" =

 -- check whether each rule has "->" separator
checkRulesSeparators :: String -> String
checkRulesSeparators x =  printSyntaxCFGinfo (all (==True) (map (isInfixOf "->") (drop 3 (lines (x)))))
