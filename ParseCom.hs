module ParseCom where

import Parser
import Grammar
import ParseBexpr
import ParseAexpr

-- Parse Com

parseStatement :: Parser Com
parseStatement = sequencingCom
              <|> statement

statement :: Parser Com
statement =  ifCom
         <|> whileCom
         <|> skipCom
         <|> assignCom


sequencingCom :: Parser Com
sequencingCom = do x <- statement
                   y <- statement
                   z <- many statement
                   return (Sequencing (x : (y : z)) )

assignCom :: Parser Com
assignCom = do id <- identifier
               symbol "="
               val <- parseAexpr
               symbol ";"
               return (Assignment id val)

skipCom :: Parser Com
skipCom = do symbol "skip;"
             return (Skip)

ifCom :: Parser Com
ifCom = do x <- symbol "if"
           bex <- parseBexpr
           symbol "then"
           symbol "{"
           com1 <- parseStatement
           symbol "}"
           symbol "else"
           symbol "{"
           com2 <- parseStatement
           symbol "}"
           -- symbol "end if"
           return (If bex com1 com2)

whileCom :: Parser Com
whileCom = do symbol "while"
              bex <- parseBexpr
              symbol "do"
              symbol "{"
              com <- parseStatement
              symbol "}"
              -- symbol "end while"
              return (While bex com)

