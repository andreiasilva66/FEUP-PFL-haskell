import Data.List (intercalate, sortOn)
import Stack
import Data.Char
-- import Text.Parsec hiding (State)
-- import Text.Parsec.String (Parser)

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

data Aexp
  = Var String            -- Variable
  | Num Integer           -- Integer literal
  | AAdd Aexp Aexp        -- Addition
  | ASub Aexp Aexp        -- Subtraction
  | AMul Aexp Aexp        -- Multiplication
  deriving Show

data Bexp
  = BTrue                 -- True constant
  | BFalse                -- False constant
  | Eq Aexp Aexp          -- Equality
  | BLe Aexp Aexp          -- Less than or equal to
  | Not Bexp              -- Logical negation
  deriving Show

data Stm
  = Assign String Aexp    -- Assignment
  | Seq [Stm]             -- Sequence of statements
  | If Bexp [Stm] [Stm]   -- If-then-else statement
  | While Bexp [Stm]      -- While loop
  deriving Show

type Program = [Stm]

-- Compiler functions
compA :: Aexp -> Code
compA (Var x) = [Fetch x]
compA (Num n) = [Push n]
compA (AAdd a1 a2) = compA a1 ++ compA a2 ++ [Add]
compA (ASub a1 a2) = compA a2 ++ compA a1 ++ [Sub]
compA (AMul a1 a2) = compA a1 ++ compA a2 ++ [Mult]

compB :: Bexp -> Code
compB BTrue = [Tru]
compB BFalse = [Fals]
compB (Eq a1 a2) = compA a1 ++ compA a2 ++ [Equ]
compB (BLe a1 a2) = compA a2 ++ compA a1 ++ [Le]
compB (Not b) = compB b ++ [Neg]

compile :: Program -> Code
compile [] = []
compile (stmt:rest) = case stmt of
  Assign var expr -> compA expr ++ [Store var] ++ compile rest
  Seq s1 -> compile s1 ++ compile rest
  If cond thenStm elseStm -> compB cond ++ [Branch (compile thenStm) (compile elseStm)] ++ compile rest
  While cond body -> [Loop (compB cond) (compile body)] ++ compile rest


lexer :: String -> [String]
lexer [] = []
lexer (c:cs)
  | isDigit c =
    let (num, rest) = span isDigit (c:cs)
     in num : lexer rest
  | c == ';' || c == '(' || c == ')' = [c] : lexer cs
  | isSpace c = lexer cs
  | otherwise =
    let (word, rest) = span (\x -> not (isSpace x) && x `notElem` [';', '(', ')']) (c:cs)
     in word : lexer rest

-- Parser for integers
parseInt :: String -> (Integer, String)
parseInt s = (read numStr, rest)
  where
    (numStr, rest) = span isDigit s

-- Parser for variable names
parseVar :: [String] -> (String, [String])
parseVar (c:cs) | all isAlpha c = (c, cs)
                | otherwise = error "Invalid variable name"
parseVar [] = error "Unexpected end of input"

-- Parser for arithmetic expressions
parseAexp :: [String] -> (Aexp, [String])
parseAexp ("(":rest) = parseAexpInParens rest
parseAexp (c:rest)
  | all isDigit c = (Num (read c), rest)
  | all isAlpha c = (Var c, rest)
  | otherwise = error "Invalid arithmetic expression"
parseAexp _ = error "Invalid arithmetic expression"

parseAexpInParens :: [String] -> (Aexp, [String])
parseAexpInParens s = case parseAexp s of
  (aexp, ")":rest) -> (aexp, rest)
  _ -> error "Mismatched parentheses in arithmetic expression"

-- Parser for boolean expressions
parseBexp :: [String] -> (Bexp, [String])
parseBexp ("True":rest) = (BTrue, rest)
parseBexp ("False":rest) = (BFalse, rest)
parseBexp ("not":rest) = case parseBexp rest of
  (bexp, after) -> (Not bexp, after)
parseBexp ("(":rest) = parseBexpInParens rest
parseBexp _ = error "Invalid boolean expression"

parseBexpInParens :: [String] -> (Bexp, [String])
parseBexpInParens s = case parseBexp s of
  (bexp, ")":rest) -> (bexp, rest)
  _ -> error "Mismatched parentheses in boolean expression"

-- Parser for statements
parseStm :: [String] -> (Stm, [String])
parseStm ("if":rest) = parseIf rest
parseStm ("while":rest) = parseWhile rest
parseStm (var:":=":rest) = case parseAexp rest of
  (expr, after) -> (Assign var expr, after)
parseStm _ = error "Invalid statement"

-- Parser for assignment statements
parseAssignment :: [String] -> (Stm, [String])
parseAssignment s = case parseVar s of
  (var, ":=":rest) ->
    case parseAexp rest of
      (expr, after) -> (Assign var expr, after)
  _ -> error "Invalid assignment statement"

parseSequence :: [String] -> ([Stm], [String])
parseSequence s = case parseStm s of
  (stmt, rest) -> case rest of
    (";":rest') -> case parseSequence rest' of
      (stmts, rest'') -> (stmt : stmts, rest'')
    _ -> ([stmt], rest)

parseIf :: [String] -> (Stm, [String])
parseIf s =
  case parseBexp s of
    (cond, rest) ->
      case parseStm rest of
        (thenBranch, rest') ->
          case rest' of
            ("else":rest'') ->
              case parseStm rest'' of
                (elseBranch, rest''') -> (If cond [thenBranch] [elseBranch], rest''')
            _ -> error "Invalid if-then-else statement"
        -- _ -> error "Invalid if-then-else statement"

parseWhile :: [String] -> (Stm, [String])
parseWhile s =
  case parseBexp s of
    (cond, rest) ->
      case parseStm rest of
        (body, rest') -> (While cond [body], rest')

-- Parser for the entire program
parseProgram :: [String] -> (Program, [String])
parseProgram s = parseSequence s

parse :: String -> Program
parse input =
  case parseProgram (lexer input) of
    (program, remaining) ->
      if null remaining
        then program
        else error $ "Parsing error. Remaining input: " ++ unwords remaining

-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_, stack, state) = run (compile (parse programCode), createEmptyStack, createEmptyState)

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
  print(lexer "x := 55; (x := x) - 1 and 3;")