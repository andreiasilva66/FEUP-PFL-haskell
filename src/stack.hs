module Stack (Stack, StackElement(IntElem, BoolElem),-- exportar o tipo
        push, pop, top, -- e as operações
        empty, isEmpty) where

data StackElement = BoolElem Bool | IntElem Integer deriving Show

type Stack = [StackElement]

push :: StackElement -> Stack -> Stack
push x xs = x : xs

pop :: Stack -> Stack
pop (_:xs) = xs
pop _      = error "Stack.pop: empty stack"

top :: Stack -> StackElement
top (x:_) = x
top _     = error "Stack.top: empty stack"

empty :: Stack
empty = []

isEmpty :: Stack -> Bool
isEmpty [] = True
isEmpty _  = False

-- operaçoes

performAddition :: StackElement -> StackElement -> StackElement
performAddition (IntElem x) (IntElem y) = IntElem (x + y)
performAddition _ _ = error "Stack.performAddition: not integers"