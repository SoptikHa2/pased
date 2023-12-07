{-# LANGUAGE FlexibleContexts #-}

module Sed where

import Control.Monad.State ( State, MonadState, modify, state, get, evalState )

newtype Script = Script [Line]

data LineLabel = Start
               | SpecificLine Int
               | End
               deriving (Show)

data Line = Line LineLabel [Command]
            deriving (Show)

data Command = Label String
             | Goto String
             | Swap
             | Print String
             | Exit
             | Substitute String String
             | GotoIfSubstituted String
             | HoldToPattern
             | AppendHoldToPattern
             | PatternToHold
             | AppendPatternToHold
             | PrintLinePattern
             deriving (Show)


-- Idea:
-- There are variables.
-- Variables are stored in the hold space.
-- Format: #<name>:<value>#<name>:<value>#...
-- Special characters that cannot occur in name nor value: :, \n, #, !. Those will be replaced by "!0", "!1", "!2", "!3".
-- All values are strings, however, if int-like, arithmetic operations are possible.
--
-- When we need to work with a variable, we move it into the pattern space:
-- 1. Hold space to pattern space
-- 2. Remove everything from the pattern space except for the variable value.
-- 3. Manipulate the value
-- 4. Copy the new value into the hold space
-- 5. Replace old value of the variable with the new contents
--
-- The name of the variable that we need to work with has to be known at compile time.
--
-- Conditions:
-- We jump to a label if the variable content is not 0/empty.
-- We try to replace the variable's value by its own value, but will only match if nonzero/nonempty.
-- Afterwards, GotoIfSubstituted.

-- INT: ID of last goto label generated
type SedState = Int

incLabel :: SedState -> (String, SedState)
incLabel state = ("jmp" ++ show state, state + 1)

-- Create new variable if it does not exist yet, or set it to a new value if it does.
setVar :: MonadState SedState m => String -> String -> m [Command]
setVar name value = do
    newJumpTarget <- state incLabel
    return [
        Swap, -- work on hold space
        Substitute ("#" ++ name ++ ":[^#]*") ("#" ++ name ++ ":" ++ value), -- try to substitute
        GotoIfSubstituted newJumpTarget, -- if substitution was successful, jump forward
        Substitute "$" ("#" ++ name ++ ":" ++ value), -- if substitution was not successful, append new variable
        Label newJumpTarget, -- here is end of the important part
        Swap -- return
        ]

-- True if variable is not zero / not empty
condition :: MonadState SedState m => String -> [Command] -> [Command] -> m [Command]
condition variableToCheck trueBody falseBody = do
    elseLabel <- state incLabel
    joinLabel <- state incLabel
    trashLabel <- state incLabel
    return ([
        Swap,
        -- clear gotoIfSubstituted flag
        GotoIfSubstituted trashLabel,
        Label trashLabel,
        -- if
        Substitute ("(#" ++ variableToCheck ++ ":0?($|#))") "YES|\\1", -- check if is zero or empty
        Swap,
        GotoIfSubstituted elseLabel -- goto else if false
        ] ++ trueBody ++ [ -- here execution resumes if truthful
        Goto joinLabel, -- jump over else
        Label elseLabel -- here execution resumes if not truthful
        ] ++ falseBody ++ [
        Label joinLabel -- here execution resumes after if
        ])

printHold :: MonadState SedState m => m [Command]
printHold = return [
        Swap,
        PrintLinePattern,
        Swap
    ]

example :: MonadState SedState m => m [Command]
example = do
    firstSet <- setVar "x" "5"
    firstPrint <- printHold
    secondSet <- setVar "x" "6"
    secondPrint <- printHold
    return $ firstSet ++ firstPrint ++ secondSet ++ secondPrint

setVarExample :: [Command]
setVarExample = evalState example 0

conditionExample :: [Command]
conditionExample = evalState condEx 0
    where condEx = do
            v1 <- setVar "x" "3"
            dbg <- printHold
            cond <- condition "x" [Print "TRUE"] [Print "FALSE"]
            return $ v1 ++ dbg ++ cond

-- >>> flip evalState 0 $ setVar "x" "5"
-- [Swap,Substitute "|x:[^|.]*" "|x:5",GotoIfSubstituted "jmp0",Substitute "$" "|x:5",Label "jmp0",Swap]

-- >>> "jmp" ++ $ show 0
-- "jmp0"

--setVar name value = [
--        Swap, -- work on hold space
--        Substitute ("|" ++ name ++ ":[^|.]*") ("|" ++ name ++ ":" ++ value),
--        Swap -- return
--    ]
