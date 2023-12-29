import Data.List (intercalate, sortOn)
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
  | otherwise = case top s of
                  IntElem x  -> show x
                  BoolElem b -> show b
                ++ if isEmpty (pop s) then "" else "," ++ stack2Str (pop s)

createEmptyState :: State
createEmptyState = []

state2Str :: State -> String
state2Str state = intercalate "," (map element2Str (sortByName state))
  where
    sortByName :: State -> State
    sortByName = sortOn fst

    element2Str :: (String, StackElement) -> String
    element2Str (name, element) = name ++ "=" ++ case element of
                                                  IntElem x  -> show x
                                                  BoolElem b -> show b

-- vai buscar o int no topo da stack
getInt :: Stack -> Integer
getInt stack = case top stack of
  IntElem x -> x
  _         -> error $ "Run-time error" 


-- vai buscar a bool no topo da stack
getBool :: Stack -> Bool
getBool stack = case top stack of
  BoolElem x -> x
  _          -> error $ "Run-time error" 

-- ve se sao iguais
equal :: Stack -> Bool
equal stack = case (top stack, top (pop stack)) of
  (IntElem x, IntElem y) -> x == y
  (BoolElem x, BoolElem y) -> x == y
  _                      -> error $ "Run-time error" 


-- ve se e menor ou igual
lessEqual :: Integer -> Integer -> Bool
lessEqual x y = x <= y


-- 
fetch :: String -> State -> Stack -> Stack
fetch x state stack = case lookup x state of
  Just x -> push x stack
  Nothing -> error $ "Run-time error" 


store :: String -> State -> Stack -> State
store x st stack =
  case top stack of
    IntElem val -> updateState (x, IntElem val) st
    BoolElem val -> updateState (x, BoolElem val) st
  where
    updateState :: (String, StackElement) -> State -> State
    updateState entry state =
      case lookup (fst entry) state of
        Just _  -> entry : filter (\(name, _) -> name /= fst entry) state
        Nothing -> entry : state

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
run (Branch c1 c2: code,(BoolElem boole:stackRest),state)
                                                    | boole == True = run (c1 ++ code,stackRest,state)
                                                    | otherwise = run (c2 ++ code,stackRest,state)
run (Loop c1 c2: code,stack,state) = run ((c1 ++ [Branch (c2 ++ [Loop c1 c2]) [Noop]]) ++ code,stack,state)
run ( Noop :rest, stack, state) = run (rest, stack, state)


testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)


main :: IO ()
main = do
  print(testAssembler [Tru,Tru,Store "y", Fetch "x",Tru])


-- Examples:
-- testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
-- testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
-- testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
-- testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
-- testAssembler [Push (-20),Push (-21), Le] == ("True","")
-- testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
-- testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")
-- If you test:
-- testAssembler [Push 1,Push 2,And]
-- You should get an exception with the string: "Run-time error"
-- If you test:
-- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
-- You should get an exception with the string: "Run-time error"



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
