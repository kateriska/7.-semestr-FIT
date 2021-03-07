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

-------------------------------------------------------------------------------------------------
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

    --print (grammar_input_transformed)

    print (processAlgorithms (switch) (grammar_input_transformed))

    --let algorithm1 = processAlgorithm1 grammar_input_transformed
    --print (algorithm1)

    --let algorithm2 = processAlgorithm2 algorithm1
    --print (algorithm2)

  ------------------------------------------------------------------------

    return ()
