module Lib
    ( someFunc
    ) where

import Pascal
import Sed
import Emit

examplePascal :: Statement
examplePascal = Block [Assign "x" (IntConst 1),
                       Assign "y" (IntConst 2),
                       Assign "z" (IntConst 3),
                       If (VarRead "x")
                          (Assign "y" (IntConst 4))
                          (Assign "z" (IntConst 5)),
                       While (VarRead "x")
                             (Assign "x" (IntConst 0)),
                       Throwaway $ FuncCall "print" [VarRead "x"]]


someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- >>> import Pascal
-- >>> Skip
-- Skip

-- >>> emit $ Script [Line Start setVarExample]
-- "1 { x ; s/#x:[^#]*/#x:5/ ; tjmp0 ; s/$/#x:5/ ; :jmp0 ; x ; x ; p ; x ; x ; s/#x:[^#]*/#x:6/ ; tjmp1 ; s/$/#x:6/ ; :jmp1 ; x ; x ; p ; x ; }\n"
