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

    arguments <- getArgs

    let (switch, file) = checkArguments arguments

    grammar_input <- readGrammarInput file

    -- check rules "->" separator before transforming to CFG data type
    let check_arrow_rules = checkRulesSeparators grammar_input

    -- insert input to CFG structure
    let grammar_input_transformed = parseCFG grammar_input
    -- validate syntax analysis of input CFG structure
    let check = checkSyntaxCFG grammar_input_transformed

    let cfg_info_check = printSyntaxCFGinfo check

    -- process 4.5 or 4.7 algorithm or only show input validated grammar
    print (processAlgorithms (switch) (grammar_input_transformed))

    return ()
