# Programação Functional e em Lógica

This Haskell project simulates the execution of a low-level machine with configurations represented as tuples (c, e, s), where c is a list of instructions (code), e is the evaluation stack, and s is the storage. The project also includes a small imperative programming language with arithmetic and boolean expressions, supporting assignments, sequences of statements, conditional (if-then-else) statements, and while loops.

## Authors
- Group T11_G06
- Andreia Filipa Gonçalves Silva (up202108769) - Participation: 50%
- Miguel Morais Dionísio (up202108788) - Participation: 50%

## Execution
To compile the program and run it, execute in the src folder:
```
ghc -o program stack.hs main.hs
./program
```
## Stack module

It follows a type-safe approach by utilizing algebraic data types (StackElement) and pattern matching in functions to handle different cases. Error handling is integrated into the functions, throwing run-time errors for certain invalid operations (e.g., popping from an empty stack).

### Data and Types
``` data StackElement = BoolElem Bool | IntElem Integer deriving Show ```
Represents elements that can be stored in the stack. Two possible variants: BoolElem for boolean elements and IntElem for integer elements. The deriving Show clause allows the elements to be displayed.

``` type Stack = [StackElement] ```
Represents a stack of elements. Is represented by a list of StackElements.

### Functions

This module includes the common stack functions: push, pop, top, empty, isEmpty and top specific functions depending on the data expected ```getInt getBool```.

## Part 1 

### Data and types
The data (Inst) and types (Code, State) to model the stack, instructions, and program state.
Functions are defined to manipulate stacks, states, and execute instructions.
```
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show

type Code = [Inst]

type State = [(String, StackElement)]
```

### Functions 

- ``` createEmptyStack :: Stack ```
Calls the ```empty``` function of module stack to create a empty stack.

- ``` stack2Str :: Stack -> String ```
Converts a stack to a string for display purposes. Checks if the stack is empty and handles different cases based on the type of the top element.

- ``` createEmptyState :: State ```
Creates an empty state (empty list).

- ``` state2Str :: State -> String ```

Converts a state to a string for display purposes. Sorts the state by variable names. Uses intercalate to concatenate variable bindings into a comma-separated string.

- ``` equal :: Stack -> Bool ```
Checks if the top two elements on the stack are equal.
Throws a runtime error if the types do not match.

- ``` lessEqual :: Integer -> Integer -> Bool ```
Compares two integers and checks if the first is less than or equal to the second.

- ``` fetch :: String -> State -> Stack -> Stack ```
Fetches a variable from the state and pushes its value onto the stack.
Throws a runtime error if the variable is not found in the state.

- ``` store :: String -> State -> Stack -> State ```
Stores the top element of the stack in the state as a variable.

- ``` run :: (Code, Stack, State) -> (Code, Stack, State) ```
Executes a list of instructions (Code) given the current stack and state.
Uses pattern matching to handle different instructions, updating the stack and state accordingly.
Handles arithmetic operations, boolean operations, variable fetching/storing, and control flow instructions (branching and looping).
Returns a tuple containing the remaining instructions, the updated stack, and the updated state. 

## Part 2

In the second part of TP2, our objective is to define a compiler for a small imperative programming language.

### Data and Types

``` data Aexp = Num Integer | Var String | AddA Aexp Aexp | SubA Aexp Aexp | MultA Aexp Aexp  deriving Show ``` for arithmetic expressions.
``` data Bexp = EquB Aexp Aexp | LeB Aexp Aexp | AndB Bexp Bexp | EquBoolB Bexp Bexp | NegB Bexp | TruB | FalsB  deriving Show ``` for boolean expressions.
``` data Stm = BranchS Bexp [Stm] [Stm] | LoopS Bexp [Stm] | VarAssign String Aexp deriving Show ``` for statements
``` type Program = [Stm] ``` uma lista de statements.

### Functions

``` lexer :: String -> [String] ```
Separates every tokent in the string storing them in a list of strings. Uses an auxiliar function to recursively go through the string, if it doesnt match any of the function calls, it's considered a Var or integer.

``` compile :: [Stm] -> Code ```
 This function compiles a program (a list of statements) into Code.

``` compA :: Aexp -> Code ```
This function compiles an arithmetic expression (Aexp) into a sequence of instructions (Code).
    If the Aexp is Num, it pushes it into the stack.
    If the Aexp is a Var, it fetches its value.
    If Aexp is an addition, subtraction, or multiplication, it compiles both Aexps and adds an Add, Sub, or Mult instruction to the Code.

``` compB :: Bexp -> Code ```
 Compiles a boolean expression (Bexp) into a sequence of instructions (Code).

``` cmpOneStm :: Stm -> Code ```
 Compiles an individual statement (Stm) into a Code.

``` parse :: String -> Program ```
 Parses a string representation of a program and returns the corresponding program structure.

``` getJustvalueBexp :: Maybe Bexp -> Bexp ```
 Extracts the Bexp from a Maybe value, throwing an error if it is nothing.

``` parMatch :: [String] -> [String] ```
 This function checks if a list of strings starts with a parenthesis and returns the remaining strings without the parenthesis.

```getFirstChar :: [String] -> String```
 Returns the first element of a list of strings.

```parseInt :: [String] -> Maybe(Aexp,[String])``
 Parses a list of strings into an Aexp or a Var.

```parseMult :: [String] -> Maybe(Aexp,[String])```
 This function parses a list of strings into a product or an Aexp.

```parseAddSub :: [String] -> Maybe(Aexp,[String])```
 This function parses a list of strings into a sum, difference, or an Aexp.

```parseParent :: [String] -> Maybe (Aexp,[String])```
 This function parses a list of strings into an Aexp or a parenthesized expression.

```parseParentMult :: [String] -> Maybe (Aexp,[String])```
 This function parses a list of strings into a product, an Aexp, or a parenthesized expression.

```parseParentSum :: [String] -> Maybe (Aexp,[String])```
 This function parses a list of strings into a sum, difference, an arithmetic expression, or a parenthesized expression.

``` parseAllBexp :: [String] -> Maybe (Bexp,[String]) ```
This function parses a list of strings into a boolean expression, handling different cases such as parentheses, boolean literals, and comparison operators.
