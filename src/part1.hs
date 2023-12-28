import Data.List (intercalate)
import Stack
    ( Stack, StackElement (BoolElem), push, pop, top, empty, isEmpty )

data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show

type Code = [Inst]

type State = [(String, StackElement)]

createEmptyStack :: Stack 
createEmptyStack = empty

stack2Str :: Stack -> String
stack2Str s
  | isEmpty s = ""
  | otherwise = show (top s) ++ if isEmpty (pop s) then "" else "," ++ stack2Str (pop s)

createEmptyState :: State
createEmptyState = []

state2Str :: State -> String
state2Str state = intercalate "," (map element2Str state)
  where
    element2Str :: (String, StackElement) -> String
    element2Str (name, element) = name ++ "=" ++ show element




run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run (inst:rest, stack, state) = case inst of
  Add -> run (rest, performOperation (+) stack, state)
  Sub -> run (rest, performOperation (-) stack, state)
  Mult -> run (rest, performOperation (*) stack, state)
  Equ -> run (rest, performComparison (==) stack, state)
  Le -> run (rest, performComparison (<=) stack, state)
  _ -> error "Invalid"


performOperation :: (StackElement -> StackElement -> StackElement) -> Stack -> Stack
performOperation op stack = push (op (top stack) (top (pop stack))) (pop (pop stack))

performComparison :: (StackElement -> StackElement -> Bool) -> Stack -> Stack
performComparison op stack = push (BoolElem (op (top (pop stack)) (top stack))) (pop (pop stack))

