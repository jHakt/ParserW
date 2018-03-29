module ParserW where

import Parser
import Grammar
import ParseCom
import Data.List

-- Main 

parseString :: String -> Com
parseString x = case (parse parseStatement x) of
                     [(com, [])] -> com
                     [(_, out)] -> error ("Parsing error near: " ++ out)
                     [] -> error ("Parsing error in the first statement of your code: \n\"" ++ x ++ "\"")


-- anyString :: String -> String -> [String] -> [String]
-- anyString [] [] [] = []
-- anyString [] [] xxs = xxs
-- anyString (x:xs) ys xxs 
--              | (isInfixOf "if" ys) && x == ';' = anyString xs (ys ++ (x : [])) xxs
--              | (isInfixOf "if" ys) && x == 'f' && (isInfixOf "end i" ys) = anyString xs "" (xxs ++ ((ys ++ (x : [])) : []))
--              | (isInfixOf "while" ys) && x == ';' = anyString xs (ys ++ (x : [])) xxs
--              | (isInfixOf "while" ys) && x == 'e' && (isInfixOf "end whil" ys) = anyString xs "" (xxs ++ ((ys ++ (x : [])) : []))
--              | x == ';' = anyString xs "" (xxs ++ ((ys ++ (x : [])) : []) )
--              | otherwise = anyString xs (ys ++ (x : [])) xxs


 
            
          