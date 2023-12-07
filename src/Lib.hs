module Lib
    ( test
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


-- >>> import Pascal
-- >>> Skip
-- Skip

test = emit $ Script [Line Start conditionExample]

-- >>> emit $ Script [Line Start setVarExample]
-- "1 { x ; s/#x:[^#]*/#x:5/ ; tjmp0 ; s/$/#x:5/ ; :jmp0 ; x ; x ; p ; x ; x ; s/#x:[^#]*/#x:6/ ; tjmp1 ; s/$/#x:6/ ; :jmp1 ; x ; x ; p ; x ; }\n"

-- >>> emit $ Script [Line Start conditionExample]
-- "1 { x ; s/#x:[^#]*/#x:3/ ; tjmp0 ; s/$/#x:3/ ; :jmp0 ; x ; x ; p ; x ; x ; s/(#x:0?($|#))/YES|\\1/ ; x ; tjmp1 ; i\\nTRUE\n ; bjmp2 ; :jmp1 ; i\\nFALSE\n ; :jmp2 ; }\n"
