import Data.List (intercalate, sortOn)
import Stack
import Data.Char

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
  | BAnd Bexp Bexp         -- Logical and
  deriving Show

data Stm
  = Assign String Aexp    -- Assignment
  | Seq [Stm]             -- Sequence of statements
  | If Bexp [Stm] [Stm]   -- If-then-else statement
  | While Bexp [Stm]      -- While loop
  deriving Show

type Program = [Stm]

-- Compiler functions

-- Function to compile arithmetic expressions (Aexp) into stack machine code (Code).
compA :: Aexp -> Code
compA (Var x) = [Fetch x]          -- Fetch the value of a variable from the state.
compA (Num n) = [Push n]           -- Push a numeric literal onto the stack.
compA (AAdd a1 a2) = compA a1 ++ compA a2 ++ [Add]    -- Compile addition operation.
compA (ASub a1 a2) = compA a2 ++ compA a1 ++ [Sub]    -- Compile subtraction operation.
compA (AMul a1 a2) = compA a1 ++ compA a2 ++ [Mult]   -- Compile multiplication operation.

-- Function to compile boolean expressions (Bexp) into stack machine code (Code).
compB :: Bexp -> Code
compB BTrue = [Tru]               -- Push True onto the stack.
compB BFalse = [Fals]              -- Push False onto the stack.
compB (Eq a1 a2) = compA a1 ++ compA a2 ++ [Equ]    -- Compile equality check.
compB (BLe a1 a2) = compA a2 ++ compA a1 ++ [Le]    -- Compile less than or equal check.
compB (Not b) = compB b ++ [Neg]   -- Compile boolean negation.

-- Function to compile a program into code.
compile :: Program -> Code
compile [] = []  -- Base case: Empty program.
compile (stmt:rest) = case stmt of
  Assign var expr -> compA expr ++ [Store var] ++ compile rest  -- Compile assignment.
  Seq s1 -> compile s1 ++ compile rest               -- Compile sequence of statements.
  If cond thenStm elseStm ->
    compB cond ++ [Branch (compile thenStm) (compile elseStm)] ++ compile rest  -- Compile if statement.
  While cond body ->
    [Loop (compB cond) (compile body)] ++ compile rest           -- Compile while loop.

-- Function to tokenize a string into a list of lexemes.
lexer :: String -> [String]
lexer "" = []  -- Base case: Empty string results in an empty list of lexemes.
lexer ('w':'h':'i':'l':'e':rest) = "while" : lexer rest  
lexer ('i':'f':rest) = "if" : lexer rest  
lexer ('t':'h':'e':'n':rest) = "then" : lexer rest  
lexer ('e':'l':'s':'e':rest) = "else" : lexer rest 
lexer ('*':rest) = "*" : lexer rest  
lexer ('+':rest) = "+" : lexer rest  
lexer ('/':rest) = "/" : lexer rest  
lexer ('-':rest) = "-" : lexer rest 
lexer (';':rest) = ";" : lexer rest  
lexer ('(':rest) = "(" : lexer rest  
lexer (')':rest) = ")" : lexer rest 
lexer ('<':'=':rest) = "<=" : lexer rest  
lexer ('=':'=':rest) = "==" : lexer rest  
lexer ('n':'o':'t':rest) = "not" : lexer rest  
lexer ('=':rest) = "=" : lexer rest 
lexer ('a':'n':'d':rest) = "and" : lexer rest  
lexer (':':'=':rest) = ":=" : lexer rest  
lexer ('d':'o':rest) = "do" : lexer rest  
lexer (' ':rest) = lexer rest  
lexer (a:rest) = lexeracc (a:rest) [] 

-- Helper function for accumulating characters until a separator is encountered.
lexeracc :: String -> String -> [String]
lexeracc "" stracc | stracc == "" = []  -- If no accumulated characters, return an empty list.
                   | otherwise = [stracc]  -- If there are accumulated characters, return them as a single-element list.
lexeracc (' ':rest) stracc | stracc == "" = lexer rest  -- Skip spaces and continue tokenizing.
                          | otherwise = stracc : lexer rest  -- If accumulated characters, add them to the list and continue tokenizing.
lexeracc (a:rest) stracc = lexeracc rest (stracc ++ [a])  -- Continue accumulating characters.


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
parseAexp ("(":rest) = (parseAexpInParens rest)
parseAexp (c:rest)
  | all isDigit c = (Num (read c), rest)
  | all isAlpha c = (Var c, rest)
  | c == "-" = case parseAexp rest of
                 (a1, rest') -> (ASub (Num 0) a1, rest')
  | otherwise = error "Invalid arithmetic expression"
parseAexp _ = error "Invalid arithmetic expression"

parseAexpInParens :: [String] -> (Aexp, [String])
parseAexpInParens s = case parseAexp s of
  (aexp, rest) -> (aexp, rest)
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
-- parseProgram :: [String] -> (Program, [String])
-- parseProgram s = parseSequence s

parse :: String -> Program
parse input =
  case parseSequence (lexer input) of
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
  print (testParser "x := 5; x := x - 1;" == ("","x=4"))
  print (testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2"))
  print (testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1"))
  print (testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2"))
  print (testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4"))
  print (testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6"))
  print (testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;)" == ("","fact=3628800,i=1"))