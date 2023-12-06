module Pascal where

type ID = String 

data Expression = VarRead ID
                | IntConst Int
                | StringConst String
                | FuncCall ID [Expression]
                | Arithmetics Expression ArithmeticsOperator Expression
                | Comparison Expression ComparisonOperator Expression
                deriving (Show)

data Statement = Assign ID Expression
               | If Expression Statement Statement
               | While Expression Statement
               | Block [Statement]
               | Skip
               | Throwaway Expression
               deriving (Show)

data ArithmeticsOperator = Plus
                         | Minus
                         | Times
                         | Div
                         | StrAppend
                         deriving (Show)

data ComparisonOperator = Less
                        | LessEq
                        | Greater
                        | GreaterEq
                        | Eq
                        | NotEq
                        deriving (Show)
