module  Stack where

type alias Stack = List Float
type alias Operator = Float -> Float -> Float

push: Float -> Stack -> Stack
push  number stack =
  number :: stack

pop: Stack -> (Float, Stack)
pop stack =
  case stack of
    [] -> (0, stack)
    head::tail -> (head, tail)

calc: Stack -> Operator -> Stack
calc stack operation =
  let
    (val1, stack1) = pop stack
    (val2, stack2) = pop stack1
  in
    push (operation val2 val1) stack2
