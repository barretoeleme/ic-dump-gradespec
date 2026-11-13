{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Main where

import Data.Kind (Type)
import PEG (parse)
import PEG (Result(OK))
import PEG.QQ (pegGrammar)

data OutputElement 
  = StaticText String
  | CalculatedValue String
  deriving (Show, Eq)

[pegGrammar|
    %name parseOutput
    %stream String
    %start specification

    specification :: [OutputElement] <- elems:element* { elems }
    element :: OutputElement <- calc:calculatedValue { calc } / stat:staticText { stat }
    calculatedValue :: OutputElement <- "{{" content:[^}]+ "}}" { CalculatedValue content }
    staticText :: OutputElement <- content:[^{]+ { StaticText content }
|]

main :: IO ()
main = do
    let example = "Sales report: Client {{ nameClient }} Total: $ {{ totalValue }}"
    putStrLn "Processing..."
    
    case parse parseOutput example of
        OK res _ _ -> do
            putStrLn "Extracted elements"
            mapM_ print res
        err -> do
            putStrLn "Error in parsing."
            print err