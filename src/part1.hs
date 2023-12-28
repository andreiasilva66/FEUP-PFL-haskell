import Data.List (intercalate)
import Stack

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


-- vai buscar o int no topo da stack
getInt :: Stack -> Integer
getInt stack = case top stack of
  IntElem x -> x
  _         -> error "getInt: expected an integer on top of the stack"


-- vai buscar a bool no topo da stack
getBool :: Stack -> Bool
getBool stack = case top stack of
  BoolElem x -> x
  _          -> error "getBool: expected a boolean on top of the stack"


-- ve se sao iguais
equal :: Stack -> Bool
equal stack = case (top stack, top (pop stack)) of
  (IntElem x, IntElem y) -> x == y
  (BoolElem x, BoolElem y) -> x == y
  _                      -> error "equal: expected two elements of the same type on top of the stack"


-- ve se e menor ou igual
lessEqual :: Integer -> Integer -> Bool
lessEqual x y = x <= y


-- 
fetch :: String -> State -> Stack -> Stack
fetch x state stack = case lookup x state of
  Just x -> push x stack
  Nothing -> error "fetch: variable not found"


store :: String -> State -> Stack -> State
store x st stack = case top stack of
  IntElem val -> (x, IntElem (getInt stack)) : st
  BoolElem val -> (x, BoolElem (getBool stack)) : st

tt :: Bool
tt = True

ff :: Bool
ff = False

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, states) = ([], stack, states)
run ( Add :rest, stack, state) = run (rest, push (IntElem (getInt stack + getInt (pop stack))) (pop (pop stack)), state)
run ( Mult :rest, stack, state) = run (rest, push (IntElem (getInt stack * getInt (pop stack))) (pop (pop stack)), state)
run ( Sub :rest, stack, state) = run (rest, push (IntElem (getInt stack - getInt (pop stack))) (pop (pop stack)), state)
run ( Tru :rest, stack, state) = run (rest, push (BoolElem tt) stack, state)
run ( Fals :rest, stack, state) = run (rest, push (BoolElem ff) stack, state)
run ( Push x :rest, stack, state) = run (rest, push (IntElem x) stack, state)
run ( Equ :rest, stack, state) = run (rest, push (BoolElem (equal stack)) (pop (pop stack)), state)
run ( Le :rest, stack, state) = run (rest, push (BoolElem (lessEqual (getInt stack) (getInt (pop stack)))) (pop (pop stack)), state)
run ( And :rest, stack, state) = run (rest, push (BoolElem (getBool stack && getBool (pop stack))) (pop (pop stack)), state)
run ( Neg :rest , stack, state) = run (rest , push (BoolElem (not (getBool stack))) (pop stack), state)
run ( Fetch x :rest, stack, state) = run (rest, fetch x state stack, state)
run ( Store x :rest, stack, state) = run (rest, pop stack, store x state stack)
run ( Branch c1 c2 :rest, stack, state) = run(if getBool stack then run(c1,stack,state) else run(c2,stack,state))
run ( Loop c1 c2 :rest, stack, state) = run(c1,stack,state)
run ( Noop :rest, stack, state) = run (rest, stack, state)

{-
performOperation :: String -> Stack -> Stack  
performOperation "+" stack = case (top stack, top (pop stack)) of
  (IntElem x, IntElem y) -> push (IntElem (x + y)) (pop (pop stack))
  _                      -> error "performOperation: expected two integers on top of the stack"

performOperationBool :: (Bool -> Bool -> Bool) -> Stack -> Stack
performOperationBool op stack = case (top stack, top (pop stack)) of
  (BoolElem x, BoolElem y) -> push (BoolElem (op x y)) (pop (pop stack))
  _                        -> error "performOperationBool: expected two booleans on top of the stack"

--performOperation :: (StackElement -> StackElement -> StackElement) -> Stack -> Stack
--performOperation op stack = push (op (top stack) (top (pop stack))) (pop (pop stack))

-- os tres stack elements refere-se a operaçao (+)

-}
