# Programação Functional e em Lógica

## Authors
- Group T11_G06
- Andreia Filipa Gonçalves Silva (up202108769) - Participation: 50%
- Miguel Morais Dionísio (up202108788) - Participation: 50%

## Execution
To compile the program and run it, execute:
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

This module includes the common stack functions: push, pop, top...

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