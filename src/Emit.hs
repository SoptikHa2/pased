module Emit where

import Sed
import Data.List (intercalate)

emit :: Script -> String
emit (Script lines) = unlines $ map emitLine lines

emitLine :: Line -> String
emitLine (Line label commands) = emitLabel label ++ emitCommands commands

emitLabel :: LineLabel -> String
emitLabel Start = "1"
emitLabel (SpecificLine n) = show n
emitLabel End = "$"

emitCommands :: [Command] -> String
emitCommands cmd = " { " ++ intercalate " ; " (map emitCommand cmd) ++ " ; }"

emitCommand :: Command -> String
emitCommand (Label label) = ":" ++ label
emitCommand (Goto label) = "b" ++ label
emitCommand Swap = "x"
emitCommand (Print text) = "i\\\n" ++ text ++ "\n"
emitCommand Exit = "b"
emitCommand (Substitute pattern replacement) = "s/" ++ pattern ++ "/" ++ replacement ++ "/"
emitCommand (GotoIfSubstituted label) = "t" ++ label
emitCommand HoldToPattern = "g"
emitCommand AppendHoldToPattern = "G"
emitCommand PatternToHold = "h"
emitCommand AppendPatternToHold = "H"
emitCommand PrintLinePattern = "p"
