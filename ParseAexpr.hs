module ParseAexpr where

import Parser
import Grammar

-- Parse Aexpr

aexpr :: Parser Aexpr
aexpr = do e1 <- aexpr'           
           do symbol "+"
              e2 <- aexpr
              return (Add e1 e2)
            <|> do symbol "-"
                   e2 <- aexpr
                   return (Subtract e1 e2)
            <|> return (e1)

aexpr' :: Parser Aexpr
aexpr' = do t1 <- term           
            do symbol "+"
               t2 <- term
               return (Add t1 t2)
             <|> do symbol "-"
                    t2 <- term
                    return (Subtract t1 t2)
             <|> return (t1)

term :: Parser Aexpr
term = do f <- factor
          do symbol "*"
             t <- term
             return (Multiply f t)
           <|> do symbol "/"
                  t <- term
                  return (Divide f t)
           <|> return (f)

factor :: Parser Aexpr
factor = do symbol "("
            e <- aexpr
            symbol ")"
            return (e)
          <|> do x <- natural
                 return (Int (toInteger x))
          <|> do x <- identifier
                 return (VarLoc x)
          -- Carattere di terminazione "=" ">" "<" "t (then)" "d (do)" ";"

parseAexpr :: Parser Aexpr
parseAexpr =  do x <- aexpr
                 return x

-- End Aexpr