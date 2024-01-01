module Stack (
        Stack, 
        StackElement(..), -- export the constructors of StackElement
        push, pop, top, getInt, getBool,
        empty, isEmpty) -- export these functions
        where

-- Define a data type StackElement with two possible variants: BoolElem and IntElem.
-- These represent elements that can be stored in the stack, either a boolean or an integer.
data StackElement = BoolElem Bool | IntElem Integer deriving Show

-- Define a type synonym Stack, which is essentially a list of StackElements.
type Stack = [StackElement]

-- Function to push a new element onto the stack.
-- Takes a StackElement and a current Stack, and adds the new element to the top of the stack.
push :: StackElement -> Stack -> Stack
push x xs = x : xs

-- Function to pop the top element off the stack.
-- Takes the current Stack and returns a new Stack without the top element.
pop :: Stack -> Stack
pop (_:xs) = xs
pop _ = error $ "Run-time error"  -- Error if attempting to pop from an empty stack.

-- Function to extract an Integer from the top of the stack.
-- Throws a runtime error if the top of the stack is not an Integer.
getInt :: Stack -> Integer
getInt stack = case top stack of
  IntElem x -> x
  _ -> error $ "Run-time error"

-- Function to extract a Bool from the top of the stack.
-- Throws a runtime error if the top of the stack is not a Bool.
getBool :: Stack -> Bool
getBool stack = case top stack of
  BoolElem x -> x
  _ -> error $ "Run-time error"

-- Function to get the top element of the stack without removing it.
-- Takes the current Stack and returns the top element.
top :: Stack -> StackElement
top (x:_) = x
top _ = error $ "Run-time error"  -- Error if attempting to get the top from an empty stack.

-- Define an initial, empty stack.
empty :: Stack
empty = []

-- Function to check if a stack is empty.
-- Returns True if the stack is empty, False otherwise.
isEmpty :: Stack -> Bool
isEmpty [] = True
isEmpty _  = False