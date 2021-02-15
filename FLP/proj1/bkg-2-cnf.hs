import System.Environment
import Data.List.Split
import Debug.Trace

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








type Rule = (String, [String])

data CFG_t = CFG_t
  {
    nonterminal_symbols :: [String],
    terminal_symbols :: [String],
    starting_symbol :: String,
    grammar_rules :: [[[Char]]]
  } deriving (Eq, Read, Show)

parseCFG :: String -> CFG_t
parseCFG grammar_input = CFG_t {
nonterminal_symbols = splitOn "," (lines (grammar_input) !! 0),
terminal_symbols = splitOn "," (lines (grammar_input) !! 1),
starting_symbol = lines (grammar_input) !! 2,
grammar_rules = map (splitOn "->") (drop 3 (lines (grammar_input)))
}





main :: IO ()
main = do
    arguments <- getArgs

    let (switch, file) = checkArguments arguments
    print (switch, file)

    grammar_input <- readGrammarInput file

    print (grammar_input)


    let nonterminal_symbols_line = lines (grammar_input) !! 0
    let terminal_symbols_line = lines (grammar_input) !! 1
    let starting_symbol_line = lines (grammar_input) !! 2
    let grammar_rules_line = tail (lines (grammar_input))

    print (nonterminal_symbols_line)
    print(terminal_symbols_line)
    print (starting_symbol_line)
    print (grammar_rules_line)

    let grammar_input_transformed = parseCFG grammar_input

    print (grammar_input_transformed)

    --let grammar_input_transformed = parseCFG grammar_input_lines

    --print (grammar_input_tran(lines grammar_input) !! sformed)



    return ()
