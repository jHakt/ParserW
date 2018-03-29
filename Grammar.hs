module Grammar where

-- Defining Aexpr (Loc = Location)

data Aexpr = Int Integer
           | VarLoc String
           | Add Aexpr Aexpr
           | Subtract Aexpr Aexpr
           | Multiply Aexpr Aexpr
           | Divide Aexpr Aexpr
             deriving (Show)


-- Defining Bexpr

data Bexpr = BoolCostant Bool
           | Not Bexpr 
           | And Bexpr Bexpr
           | Or Bexpr Bexpr
           | Equals Aexpr Aexpr
           | Less Aexpr Aexpr
           | LessOrEqual Aexpr Aexpr
           | Greater Aexpr Aexpr
           | GreaterOrEqual Aexpr Aexpr
             deriving (Show)


-- Defining Com

data Com = Skip 
         | Assignment String Aexpr
         | Sequencing [Com]
         | If Bexpr Com Com
         | While Bexpr Com
           deriving (Show)



