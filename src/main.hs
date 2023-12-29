import Data.List (intercalate, sortOn)
import Stack
import Text.Parsec hiding (State)
import Text.Parsec.String (Parser)

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
  print(testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10",""))

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

-- TODO: Define the types Aexp, Bexp, Stm and Program
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
  | Not Bexp              -- Logical negation
  deriving Show

data Stm
  = Assign String Aexp    -- Assignment
  | Seq Stm Stm           -- Sequence of statements
  | If Bexp Stm Stm       -- If-then-else statement
  | While Bexp Stm        -- While loop
  deriving Show

type Program = [Stm]

-- Compiler functions
compA :: Aexp -> Code
compA (Var x) = [Fetch x]
compA (Num n) = [Push n]
compA (AAdd a1 a2) = compA a1 ++ compA a2 ++ [Add]
compA (ASub a1 a2) = compA a1 ++ compA a2 ++ [Sub]
compA (AMul a1 a2) = compA a1 ++ compA a2 ++ [Mult]

compB :: Bexp -> Code
compB BTrue = [Tru]
compB BFalse = [Fals]
compB (Eq a1 a2) = compA a1 ++ compA a2 ++ [Equ]
compB (Not b) = compB b ++ [Neg]

compile :: Program -> Code
compile [] = []
compile (stmt:rest) = case stmt of
  Assign var expr -> compA expr ++ [Store var] ++ compile rest
  Seq s1 s2 -> compile [s1] ++ compile [s2] ++ compile rest
  If cond thenStm elseStm -> compB cond ++ [Branch (compile [thenStm]) (compile [elseStm])] ++ compile rest
  While cond body -> [Loop (compB cond) (compile [body])] ++ compile rest


-- -- Lexer
-- lexer :: Parser [String]
-- lexer = sepBy (many1 (alphaNum <|> oneOf "+-*/=<>!&|")) spaces

-- -- Parse arithmetic expression
-- parseAexp :: Parser Aexp
-- parseAexp = do
--   tokens <- lexer
--   return $ buildAexp tokens

-- buildAexp :: [String] -> Aexp
-- buildAexp [var] = Var var
-- buildAexp [num] = Num (read num)
-- buildAexp (op : tokens)
--   | op `elem` ["+", "-", "*"] =
--     let (left, right) = splitAt (length tokens `div` 2) tokens
--      in case op of
--           "+" -> AAdd (buildAexp left) (buildAexp right)
--           "-" -> ASub (buildAexp left) (buildAexp right)
--           "*" -> AMul (buildAexp left) (buildAexp right)
--   | otherwise = error "Invalid arithmetic expression"

-- -- Parse boolean expression
-- parseBexp :: Parser Bexp
-- parseBexp = do
--   tokens <- lexer
--   return $ buildBexp tokens

-- buildBexp :: [String] -> Bexp
-- buildBexp ["true"] = BTrue
-- buildBexp ["false"] = BFalse
-- buildBexp [var] = Var var
-- buildBexp [num] = Num (read num)
-- buildBexp ["not", rest] = Not (buildBexp [rest])
-- buildBexp (op : tokens)
--   | op `elem` ["==", "<=", "&&"] =
--     let (left, right) = splitAt (length tokens `div` 2) tokens
--      in case op of
--           "==" -> Eq (buildAexp left) (buildAexp right)
--           "<=" -> Le (buildAexp left) (buildAexp right)
--           "&&" -> And (buildBexp left) (buildBexp right)
--   | otherwise = error "Invalid boolean expression"

-- -- Parse assignment statement
-- parseAssign :: Parser Stm
-- parseAssign = do
--   var <- many1 lower
--   spaces
--   _ <- char '='
--   spaces
--   expr <- lexer
--   _ <- char ';'
--   return $ Assign var (buildAexp expr)

-- -- Parse sequence of statements
-- parseSeq :: Parser Stm
-- parseSeq = do
--   stmts <- sepBy1 parseStmt (char ';')
--   return $ foldr1 Seq stmts

-- -- Parse if-then-else statement
-- parseIf :: Parser Stm
-- parseIf = do
--   _ <- string "if"
--   spaces
--   cond <- parseBexp
--   spaces
--   _ <- string "then"
--   spaces
--   thenStm <- parseStmt
--   spaces
--   _ <- string "else"
--   spaces
--   elseStm <- parseStmt
--   _ <- char ';'
--   return $ If cond thenStm elseStm

-- -- Parse while loop statement
-- parseWhile :: Parser Stm
-- parseWhile = do
--   _ <- string "while"
--   spaces
--   cond <- parseBexp
--   spaces
--   body <- parseStmt
--   _ <- char ';'
--   return $ While cond body

-- -- Parse any statement
-- parseStmt :: Parser Stm
-- parseStmt = try parseAssign <|> try parseSeq <|> try parseIf <|> try parseWhile

-- -- Parse the entire program
-- parseProgram :: Parser Program
-- parseProgram = endBy parseStmt spaces

-- To help you test your parser
-- testParser :: String -> (String, String)
-- testParser programCode = (stack2Str stack, store2Str store)
--   where (_,stack,store) = run(compile (parse programCode), createEmptyStack, createEmptyStore)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")