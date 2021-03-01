import System.Environment
import Data.List.Split
import Data.List
import Data.Tuple

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

listToTuple :: [a] -> (a,a)
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
  } deriving (Eq, Read,Show)

{--
instance Show CFG_t where
  show (CFG_t nonterminal_symbols terminal_symbols starting_symbol grammar_rules) =
    (intercalate "," nonterminal_symbols) ++ "\n" ++
    (intercalate "," terminal_symbols) ++ "\n" ++
    starting_symbol ++ "\n" ++
    (unlines (map showRule grammar_rules))

--}
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
changeLeftSideRule nonterminal (x,y) = (nonterminal, y)
--concat (map filterRules ["E","F"])

filterRules rules nonterminal = filter ((==nonterminal).fst)  rules

removeSimpleRules nonterminal rules na_set = map (changeLeftSideRule nonterminal) (filter (\n ->  checkSimpleRule n == False) (concat (map (filterRules rules) na_set)) )

--accumulateTransformedRules new_rules [] = new_rules
--accumulateTransformedRules new_rules final_set = new_rules ++ final_set
iterateList :: [String] -> [([Char], [Char])] -> [([Char], [Char])]
iterateList [x] rules = removeSimpleRules x rules (recursionNA (rules) (length (rules)) [x] [x])
iterateList (x:xs) rules = removeSimpleRules x rules (recursionNA (rules) (length (rules)) [x] [x]) ++ iterateList xs rules




main :: IO ()
main = do
  -- simmilar for all three switches ------------------------------------------------
    arguments <- getArgs

    let (switch, file) = checkArguments arguments
    print (switch, file)

    grammar_input <- readGrammarInput file

    print (grammar_input)

    let check_arrow_rules = checkRulesSeparators grammar_input
    print (check_arrow_rules)

    let grammar_input_transformed = parseCFG grammar_input

    --print (grammar_input_transformed)

    let check = checkSyntaxCFG grammar_input_transformed

    let cfg_info_check = printSyntaxCFGinfo check
    print (cfg_info_check)

    print (grammar_input_transformed)

    --let algorithm2 = removeSimpleRules grammar_input_transformed
    --print (algorithm2)

  ------------------------------------------------------------------------

    return ()
