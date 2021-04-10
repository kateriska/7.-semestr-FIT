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
import Control.Monad

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
    unless (checkRulesSeparators grammar_input) $ error "Error - Wrong separators of rules of input CFG!"

    -- insert input to CFG structure
    let grammar_input_transformed = parseCFG grammar_input

    -- validate syntax analysis of input CFG structure and throw error if input grammar is in wrong format
    unless (checkSyntaxCFG grammar_input_transformed) $ error "Error - Wrong format of input CFG!"

    -- process 4.5 or 4.7 algorithm or only show input validated grammar
    print (processAlgorithms (switch) (grammar_input_transformed))

    return ()
