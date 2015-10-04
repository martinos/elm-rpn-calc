module  Stack where

type alias Stack = List Float
type alias Operator = Float -> Float -> Float

push: Float -> Stack -> Stack
push  number stack =
  number :: stack

pop': Stack -> Maybe (Float, Stack)
pop' stack =
  case stack of
    [] -> Nothing
    head::tail -> Just (head, tail)
