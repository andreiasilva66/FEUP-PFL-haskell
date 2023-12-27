import Stack

data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show

type Code = [Inst]

type Storage = [(String, StackElement)]

type State = (Code, Stack, Storage)

createEmptyStack :: Stack 
createEmptyStack = empty

-- stack2Str :: Stack -> String
-- stack2Str s
--   | isEmpty s = ""
--   | otherwise = show (top s) ++ if isEmpty (pop s) then "" else "," ++ stack2Str (pop s)

createEmptyState :: State
createEmptyState = ([], createEmptyStack, [])