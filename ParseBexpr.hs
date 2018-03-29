module ParseBexpr where

import Parser
import Grammar
import ParseAexpr

-- Parse Bexpr

bexpr :: Parser Bexpr
bexpr = do b1 <- bexpr'           
           do symbol "and"
              b2 <- bexpr
              return (And b1 b2)
            <|> do symbol "or"
                   b2 <- bexpr
                   return (Or b1 b2)
            <|> return (b1)

bexpr' :: Parser Bexpr
bexpr' = do bt1 <- bterm           
            do symbol "and"
               bt2 <- bterm
               return (And bt1 bt2)
             <|> do symbol "or"
                    bt2 <- bterm
                    return (Or bt1 bt2)
             <|> return (bt1)

bterm :: Parser Bexpr
bterm = do symbol "not"
           bf <- bfactor
           return (Not bf)
         <|> do bf <- bfactor
                return (bf)

bfactor :: Parser Bexpr
bfactor = do symbol "("
             be <- bexpr
             symbol ")"
             return (be)
           <|> do x <- symbol "true"
                  return (BoolCostant True)
           <|> do x <- symbol "false"
                  return (BoolCostant False)
           <|> do x <- parsBAE
                  return (x)
        

parsBAE :: Parser Bexpr
parsBAE =  parsLessOrEqual
       <|> parsLess
       <|> parsEqual
       <|> parsGreater
       <|> parsGreaterOrEqual
       <|> parsError

parsEqual :: Parser Bexpr
parsEqual = do x <- parseAexpr 
               s <- symbol "=="
               y <- parseAexpr
               return (Equals x y)

parsLess :: Parser Bexpr
parsLess = do x <- parseAexpr
              s <- symbol "<"
              y <- parseAexpr
              return (Less x y)

parsLessOrEqual :: Parser Bexpr
parsLessOrEqual = do x <- parseAexpr
                     s <- symbol "<="
                     y <- parseAexpr
                     return (LessOrEqual x y)

parsGreater :: Parser Bexpr
parsGreater = do x <- parseAexpr
                 s <- symbol ">"
                 y <- parseAexpr
                 return (Greater x y)

parsGreaterOrEqual :: Parser Bexpr
parsGreaterOrEqual = do x <- parseAexpr
                        s <- symbol ">="
                        y <- parseAexpr
                        return (GreaterOrEqual x y)

-- Pointless
parsError :: Parser Bexpr
parsError = error "Sintact error in Bool Expression"

--

parseBexpr :: Parser Bexpr
parseBexpr =  do x <- bexpr
                 return x

-- End Bexpr