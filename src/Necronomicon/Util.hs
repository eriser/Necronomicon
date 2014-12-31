module Necronomicon.Util (module Necronomicon.Util.PriorityQueue, (|>), (<|)) where

import Necronomicon.Util.PriorityQueue

(|>) :: a -> (a -> b) -> b
b |> a = a b

infixl 0 |>

(<|) :: (a -> b) -> a -> b
a <| b = a b

infixl 0 <|
