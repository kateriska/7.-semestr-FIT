-- Project name: bkg-2-cnf
-- Author: Katerina Fortova
-- Login: xforto00
-- Year: 2020 / 2021

-- Module description: Main module of program

module Main (main) where

import System.Environment
import Data.List.Split
import Data.List
import Data.Tuple

import Types
import Parser
import Algorithm1
import Algorithm2

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
    --------------------------------------------------------------------------------

    --print (grammar_input_transformed)

    -- process 4.5 or 4.7 algorithm
    print (processAlgorithms (switch) (grammar_input_transformed))

    --let algorithm1 = processAlgorithm1 grammar_input_transformed
    --print (algorithm1)

    --let algorithm2 = processAlgorithm2 algorithm1
    --print (algorithm2)

  ------------------------------------------------------------------------

    return ()
