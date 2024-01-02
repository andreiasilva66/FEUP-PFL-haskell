import Data.List (intercalate, sortOn, elemIndex)
import Stack
import Data.Char
import Text.Read (readMaybe)

-- Define a data type 'Inst' representing instructions that can be executed.
-- Each constructor corresponds to a specific operation or control flow instruction.
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show

-- Define a type synonym 'Code' representing a list of instructions.
type Code = [Inst]

-- Define a type synonym 'State' representing the state of the program.
-- Strings are used to represent variable names and StackElements are used to represent variable values.
type State = [(String, StackElement)]

-- Function to create an empty stack.
createEmptyStack :: Stack
createEmptyStack = empty

-- Function to convert a stack to a string for display purposes.
stack2Str :: Stack -> String
stack2Str s
  | isEmpty s = ""
  | otherwise = case top s of
                  IntElem x  -> show x
                  BoolElem b -> show b
                ++ if isEmpty (pop s) then "" else "," ++ stack2Str (pop s)

-- Function to create an empty state (list of variable bindings).
createEmptyState :: State
createEmptyState = []

-- Function to convert a state to a string for display purposes.
state2Str :: State -> String
state2Str state = intercalate "," (map element2Str (sortByName state))
  where
    -- Helper function to sort the state by variable names.
    sortByName :: State -> State
    sortByName = sortOn fst

    -- Helper function to convert a variable value to a string.
    element2Str :: (String, StackElement) -> String
    element2Str (name, element) = name ++ "=" ++ case element of
                                                  IntElem x  -> show x
                                                  BoolElem b -> show b

-- Function to check if the top two elements on the stack are equal.
-- Throws a runtime error if the types do not match.
equal :: Stack -> Bool
equal stack = case (top stack, top (pop stack)) of
  (IntElem x, IntElem y) -> x == y
  (BoolElem x, BoolElem y) -> x == y
  _ -> error $ "Run-time error"

-- Function to check if one Integer is less than or equal to another.
lessEqual :: Integer -> Integer -> Bool
lessEqual x y = x <= y

-- Function to fetch a variable from the state and push its value onto the stack.
-- Throws a runtime error if the variable is not found in the state.
fetch :: String -> State -> Stack -> Stack
fetch x state stack = case lookup x state of
  Just val -> push val stack
  Nothing  -> error $ "Run-time error"

-- Function to store the top element of the stack in the state as a variable.
-- Throws a runtime error if the top of the stack is not an IntElem or BoolElem.
store :: String -> State -> Stack -> State
store x state stack =
  case top stack of
    IntElem val  -> updateState (x, IntElem val) state
    BoolElem val -> updateState (x, BoolElem val) state
  where
    -- Helper function to update the state with a new variable binding.
    updateState :: (String, StackElement) -> State -> State
    updateState entry state =
      case lookup (fst entry) state of
        Just _  -> entry : filter (\(name, _) -> name /= fst entry) state
        Nothing -> entry : state

-- Constants representing True and False as BoolElems.
tt :: Bool
tt = True

ff :: Bool
ff = False

-- Function to execute a list of instructions (Code) given the current stack and state.
-- Returns a tuple containing the remaining instructions, the updated stack, and the updated state.
run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, states) = ([], stack, states)  -- Base case: No more instructions to execute.
run (Add:rest, stack, state) = run (rest, push (IntElem (getInt stack + getInt (pop stack))) (pop (pop stack)), state)
run (Mult:rest, stack, state) = run (rest, push (IntElem (getInt stack * getInt (pop stack))) (pop (pop stack)), state)
run (Sub:rest, stack, state) = run (rest, push (IntElem (getInt stack - getInt (pop stack))) (pop (pop stack)), state)
run (Tru:rest, stack, state) = run (rest, push (BoolElem tt) stack, state)
run (Fals:rest, stack, state) = run (rest, push (BoolElem ff) stack, state)
run (Push x:rest, stack, state) = run (rest, push (IntElem x) stack, state)
run (Equ:rest, stack, state) = run (rest, push (BoolElem (equal stack)) (pop (pop stack)), state)
run (Le:rest, stack, state) = run (rest, push (BoolElem (lessEqual (getInt stack) (getInt (pop stack)))) (pop (pop stack)), state)
run (And:rest, stack, state) = run (rest, push (BoolElem (getBool stack && getBool (pop stack))) (pop (pop stack)), state)
run (Neg:rest, stack, state) = run (rest, push (BoolElem (not (getBool stack))) (pop stack), state)
run (Fetch x:rest, stack, state) = run (rest, fetch x state stack, state)
run (Store x:rest, stack, state) = run (rest, pop stack, store x state stack)
run (Branch c1 c2:code, BoolElem boole:stackRest, state)
  | boole == True = run (c1 ++ code, stackRest, state)  -- Branch if the condition is True.
  | otherwise = run (c2 ++ code, stackRest, state)      -- Branch if the condition is False.
run (Loop c1 c2:code, stack, state) = run ((c1 ++ [Branch (c2 ++ [Loop c1 c2]) [Noop]]) ++ code, stack, state)
run (Noop:rest, stack, state) = run (rest, stack, state)  -- No operation, continue with the next instruction.

-- Function given to help to test the assembler.
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

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

-- Part 2

-- Abstract syntax for arithmetic expressions
data Aexp
  = Num Integer          -- Integer literal
  | Var String           -- Variable
  | AddA Aexp Aexp       -- Addition
  | SubA Aexp Aexp       -- Subtraction
  | MultA Aexp Aexp      -- Multiplication
  deriving Show

-- Abstract syntax for boolean expressions
data Bexp
  = EquB Aexp Aexp       -- Equality
  | LeB Aexp Aexp        -- Less than or equal to
  | AndB Bexp Bexp       -- Logical AND
  | EquBoolB Bexp Bexp   -- Equality of boolean expressions
  | NegB Bexp            -- Logical negation
  | TruB                 -- True constant
  | FalsB                -- False constant
  deriving Show

-- Abstract syntax for statements
data Stm
  = BranchS Bexp [Stm] [Stm]  -- If-then-else statement
  | LoopS Bexp [Stm]          -- While loop
  | VarAssign String Aexp     -- Variable assignment
  deriving Show

-- Type synonym for a program (sequence of statements)
type Program = [Stm]

-- Code generation for arithmetic expressions
compA :: Aexp -> Code
compA (Num a) = [Push a]
compA (Var a) = [Fetch a]
compA (AddA a b) = compA b ++ compA a ++ [Add]
compA (SubA a b) = compA b ++ compA a ++ [Sub]
compA (MultA a b) = compA b ++ compA a ++ [Mult]

-- Code generation for boolean expressions
compB :: Bexp -> Code
compB TruB = [Tru]
compB FalsB = [Fals]
compB (AndB a b) = compB b ++ compB a ++ [And]
compB (NegB a) = compB a ++ [Neg]
compB (EquB a b) = compA b ++ compA a ++ [Equ]
compB (LeB a b) = compA b ++ compA a ++ [Le]
compB (EquBoolB a b) = compB b ++ compB a ++ [Equ]

-- Code generation for programs
-- For each statement calls the function cmpOneStm
compile :: Program -> Code
compile stms = concatMap cmpOneStm stms

-- Code generation for a single statement
cmpOneStm :: Stm -> Code
cmpOneStm stm = case stm of
  VarAssign var value -> compA value ++ [Store var]
  BranchS bexp stm1 stm2  -> compB bexp ++ [Branch (compile stm1) (compile stm2)]
  LoopS bexp stm -> [Loop (compB bexp) (compile stm)]

-- Lexical analysis: Convert a string into a list of tokens
lexer :: String -> [String]
lexer string = lexeraux string [] []

-- Helper function for lexical analysis
lexeraux :: String -> [String] -> String -> [String]
lexeraux [] listStr currentStr | currentStr == "" =  listStr
                       | otherwise = (listStr++[currentStr])
lexeraux (':':'=':rest) listStr currentStr
  | currentStr == "" = lexeraux rest (listStr++[":="]) currentStr
  | otherwise = lexeraux rest (listStr++[currentStr]++[":="]) []
lexeraux (' ':rest) listStr currentStr
  | currentStr == "" = lexeraux rest listStr []
  | otherwise = lexeraux rest (listStr++[currentStr]) []
lexeraux ('+':rest) listStr currentStr
  | currentStr == "" = lexeraux rest (listStr++["+"]) currentStr
  | otherwise = lexeraux rest (listStr++[currentStr]++["+"]) []
lexeraux ('-':rest) listStr currentStr
  | currentStr == "" = lexeraux rest (listStr++["-"]) currentStr
  | otherwise = lexeraux rest (listStr++[currentStr]++["-"]) []
lexeraux ('*':rest) listStr currentStr
  | currentStr == "" = lexeraux rest (listStr++["*"]) currentStr
  | otherwise = lexeraux rest (listStr++[currentStr]++["*"]) []
lexeraux ('i':'f':rest) listStr currentStr
  | currentStr == "" = lexeraux rest (listStr++["if"]) currentStr
  | otherwise = lexeraux rest (listStr++[currentStr]++["if"]) []
lexeraux ('t':'h':'e':'n':rest) listStr currentStr
  | currentStr == "" = lexeraux rest (listStr++["then"]) currentStr
  | otherwise = lexeraux rest (listStr++[currentStr]++["then"]) []
lexeraux ('e':'l':'s':'e':rest) listStr currentStr
  | currentStr == "" = lexeraux rest (listStr++["else"]) currentStr
  | otherwise = lexeraux rest (listStr++[currentStr]++["else"]) []
lexeraux (';':rest) listStr currentStr
  | currentStr == "" = lexeraux rest (listStr++[";"]) currentStr
  | otherwise = lexeraux rest (listStr++[currentStr]++[";"]) []
lexeraux ('(':rest) listStr currentStr
  | currentStr == "" = lexeraux rest (listStr++["("]) currentStr
  | otherwise = lexeraux rest (listStr++[currentStr]++["("]) []
lexeraux (')':rest) listStr currentStr
  | currentStr == "" = lexeraux rest (listStr++[")"]) currentStr
  | otherwise = lexeraux rest (listStr++[currentStr]++[")"]) []
lexeraux ('<':'=':rest) listStr currentStr
  | currentStr == "" = lexeraux rest (listStr++["<="]) currentStr
  | otherwise = lexeraux rest (listStr++[currentStr]++["<="]) []
lexeraux ('=':'=':rest) listStr currentStr
  | currentStr == "" = lexeraux rest (listStr++["=="]) currentStr
  | otherwise = lexeraux rest (listStr++[currentStr]++["=="]) []
lexeraux ('n':'o':'t':rest) listStr currentStr
  | currentStr == "" = lexeraux rest (listStr++["not"]) currentStr
  | otherwise = lexeraux rest (listStr++[currentStr]++["not"]) []
lexeraux ('=':rest) listStr currentStr
  | currentStr == "" = lexeraux rest (listStr++["="]) currentStr
  | otherwise = lexeraux rest (listStr++[currentStr]++["="]) []
lexeraux ('a':'n':'d':rest) listStr currentStr
  | currentStr == "" = lexeraux rest (listStr++["and"]) currentStr
  | otherwise = lexeraux rest (listStr++[currentStr]++["and"]) []
lexeraux ('w':'h':'i':'l':'e':rest) listStr currentStr
  | currentStr == "" = lexeraux rest (listStr++["while"]) currentStr
  | otherwise = lexeraux rest (listStr++[currentStr]++["while"]) []
lexeraux ('d':'o':rest) listStr currentStr
  | currentStr == "" = lexeraux rest (listStr++["do"]) currentStr
  | otherwise = lexeraux rest (listStr++[currentStr]++["do"]) []                              
lexeraux (a:rest) listStr currentStr = lexeraux rest listStr (currentStr++[a])

-- Parsing: Convert a list of tokens into a program
parse :: String -> Program
parse str = parseAcc (lexer str) []

-- Helper function for parsing
parseAcc :: [String] -> [Stm] -> [Stm]
parseAcc [] stm = stm
parseAcc (a:":=":rest) stm = let x = (getInnerValue (elemIndex ";" (a:":=":rest)))
  in case parseParentSum (drop 2 (take (x-1) (a:":=":rest))) of
    Just (expr,[]) -> parseAcc (drop x (a:":=":rest)) (stm++[(VarAssign a (expr))])
    Nothing -> error $ "Run-time error"
    _ -> error $ "Run-time error"
parseAcc ("(":rest) stm = parseAcc (drop (getInnerValue (elemIndex ")" ("(":rest))) ("(":rest)) (stm++(parseAcc (drop 1 (take ((getInnerValue (elemIndex ")" ("(":rest)))-1) ("(":rest))) []))
parseAcc (";":rest) stm = parseAcc rest stm
parseAcc ("if":rest) stm = let thenIdx = (getInnerValue (elemIndex "then" ("if":rest)))
                               elseIdx = (getInnerValue (elemIndex "else" ("if":rest)))
                               restList = (drop (elseIdx) ("if":rest))
                            in case getFirstChar restList of
                              "(" -> parseAcc (drop (getInnerValue (elemIndex ")" restList)) restList) (stm++[BranchS (getJustValueBExp ((parseAnd (parMatch (drop 1 (take (thenIdx-1) ("if":rest))))))) (parseAcc (drop thenIdx (take (elseIdx-1) ("if":rest))) []) (parseAcc (take (getInnerValue (elemIndex ")" restList)) restList ) [] )])
                              _  -> parseAcc (drop (getInnerValue (elemIndex ";" restList)) restList) (stm++[BranchS (getJustValueBExp ((parseAnd (parMatch (drop 1 (take (thenIdx-1) ("if":rest))))))) (parseAcc (drop thenIdx (take (elseIdx-1) ("if":rest))) []) (parseAcc (take (getInnerValue (elemIndex ";" restList)) restList ) [] )])
parseAcc ("while":rest) stm = let doIdx = (getInnerValue (elemIndex "do" ("while":rest)))
                                  restList = (drop (doIdx) ("while":rest))
                              in case getFirstChar restList of
                                "(" -> parseAcc (drop (getInnerValue (elemIndex ")" restList)) restList) (stm++[LoopS (getJustValueBExp ((parseAnd (parMatch (drop 1 (take (doIdx-1) ("while":rest))))))) (parseAcc (take (getInnerValue (elemIndex ")" restList)) restList ) [] )])
                                _ -> parseAcc (drop (getInnerValue (elemIndex ";" restList)) restList) (stm++[LoopS (getJustValueBExp ((parseAnd (parMatch (drop 1 (take (doIdx-1) ("while":rest))))))) (parseAcc (take (getInnerValue (elemIndex ";" restList)) restList ) [] )])

-- Parsing integer or variable expressions
parseInt :: [String] -> Maybe (Aexp,[String])
parseInt (n:rest) =
  case (readMaybe n :: Maybe Integer) of
    Just v -> Just (Num v, rest)
    Nothing -> Just (Var n,rest)
parseInt _ = Nothing

-- Parsing multiplication operations in expressions
parseMult :: [String] -> Maybe(Aexp,[String])
parseMult str =
  case parseInt str of
    Just (exp1,("*":str1)) ->
      case parseMult str1 of
        Just (exp2,str2) ->
          Just (MultA exp1 exp2,str2)
        Nothing                  -> Nothing
    result -> result

-- Parsing addition or subtraction operations in expressions
parseAddSub :: [String] -> Maybe(Aexp,[String])
parseAddSub str =
  case parseMult str of
    Just (exp1,("+":str1)) ->
      case parseAddSub str1 of
        Just (exp2,str2) ->
          Just (AddA exp1 exp2,str2)
        Nothing                  -> Nothing
    Just (exp1,("-":str1)) ->
      case parseAddSub str1 of
        Just (exp2,str2) ->
          Just (SubA exp1 exp2,str2)
        Nothing                  -> Nothing
    result -> result

-- Parsing expressions with parentheses
parseParent :: [String] -> Maybe (Aexp,[String])
parseParent rest =
  case parseParentMult rest of
    Just (exp1,("*":str1)) ->
      case parseParent str1 of
        Just (exp2,str2) -> Just (MultA exp1 exp2, str2)
        Nothing -> Nothing
    result -> result

-- Parses expressions within parentheses and handles addition or subtraction operations
parseParentSum :: [String] -> Maybe (Aexp,[String])
parseParentSum rest =
  case parseParent rest of
    Just (exp1,("+":str1)) ->
      case parseParentSum str1 of
        Just (exp2,str2) -> Just (AddA exp1 exp2, str2)
        Nothing -> Nothing
    Just (exp1,("-":str1)) ->
      case parseParentSum str1 of
        Just (exp2,str2) -> Just (SubA exp1 exp2, str2)
        Nothing -> Nothing
    result -> result

-- Parses expressions within parentheses and handles multiplication operations
parseParentMult :: [String] -> Maybe (Aexp,[String])
parseParentMult ("(":rest) =
  case parseParentSum rest of
    Just (expr,(")":str1)) -> Just (expr,str1)
    Just _ -> Nothing
    Nothing -> Nothing
parseParentMult (n:rest) =
  case (readMaybe n :: Maybe Integer) of
    Just f -> Just (Num f, rest)
    Nothing -> Just (Var n,rest)
parseParentMult _ = Nothing

-- Parses Boolean expressions with possible parentheses
parseParentBool :: [String] -> Maybe (Bexp,[String])
parseParentBool ("(":rest) =
  case parseAnd rest of
    Just (expr,(")":str1)) -> Just (expr,str1)
    Just _ -> Nothing
    Nothing -> Nothing
parseParentBool ("True":rest) = Just (TruB,rest)
parseParentBool ("False":rest) = Just (FalsB,rest)
parseParentBool rest =
  case parseParentSum rest of
    Just (exp1,("<=":str1)) ->
      case parseParentSum str1 of
        Just (exp2,str2) ->
          Just (LeB exp1 exp2, str2)
        Nothing -> Nothing
    Just (exp1,("==":str1)) ->
      case parseParentSum str1 of
        Just (exp2,str2) ->
          Just (EquB exp1 exp2, str2)
        Nothing -> Nothing
    result -> Nothing

-- Parses NOT Boolean operations
parseNot :: [String] -> Maybe(Bexp, [String])
parseNot ("not":rest) =
    case parseParentBool rest of
      Just (exp1,str1) ->
        Just (NegB exp1,str1)
      result -> result
parseNot rest = parseParentBool rest

-- Parses AND Boolean operations
parseAnd :: [String] -> Maybe(Bexp,[String])
parseAnd rest =
  case parseEqual rest of
    Just (exp1, ("and":str1)) ->
      case parseAnd str1 of
        Just (exp2, str2) ->
          Just (AndB exp1 exp2, str2)
        Nothing -> Nothing
    result -> result

-- Parses Equal Boolean operations
parseEqual :: [String] -> Maybe(Bexp, [String])
parseEqual rest =
  case parseNot rest of
    Just (exp1, ("=":str1)) ->
      case parseEqual str1 of
        Just (exp2, str2) ->
          Just (EquBoolB exp1 exp2, str2)
        Nothing -> Nothing
    result -> result
    
-- Extracts the inner value of a 'Maybe' tuple containing a 'Bexp' and remaining strings
getJustValueBExp :: Maybe (Bexp,[String]) -> Bexp
getJustValueBExp (Just (a,[")"])) = a
getJustValueBExp (Just (a,[])) = a
getJustValueBExp Nothing = error $ "Run-time error"

-- Extracts the inner numerical value from a 'Maybe' wrapped number
getInnerValue :: Num a => Maybe a -> a
getInnerValue (Just a) = a+1

-- Matches parentheses and returns the rest of the string after matching
parMatch :: [String] -> [String]
parMatch ("(":rest) = drop 1 (take (length ("(":rest)) ("(":rest))
parMatch rest = rest

-- Helper function to get the first character of a string
getFirstChar :: [String] -> String
getFirstChar (a:rest) = a

-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")

main :: IO ()
main = do
  -- print(testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10",""))
  print (testParser "x := 5; x := x - 1;" == ("","x=4"))
  print (testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2"))
  print (testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1"))
  print (testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2"))
  print (testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4"))
  print (testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6"))
  print (testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;)" == ("","fact=3628800,i=1"))