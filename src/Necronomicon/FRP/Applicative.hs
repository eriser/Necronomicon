module Necronomicon.FRP.Applicative where

(<~) :: Functor f => (a -> b) -> f a -> f b
(<~) = fmap

(~~) :: Applicative f => f (a -> b) -> f a -> f b
(~~) = (<*>)

(~>) :: Functor f => f a -> (a -> b) -> f b
(~>) = flip fmap

infixl 4 <~, ~~
infixr 4 ~>

fmap2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
fmap2 f a b = f <~ a ~~ b

fmap3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
fmap3 f a b c = f <~ a ~~ b ~~ c

fmap4 :: Applicative f => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
fmap4 f a b c d = f <~ a ~~ b ~~ c ~~ d

fmap5 :: Applicative f => (a -> b -> c -> d -> e -> ff) -> f a -> f b -> f c -> f d -> f e -> f ff
fmap5 f a b c d e = f <~ a ~~ b ~~ c ~~ d ~~ e

fmap6 :: Applicative f => (a -> b -> c -> d -> e -> ff -> g) -> f a -> f b -> f c -> f d -> f e -> f ff -> f g
fmap6 f a b c d e f' = f <~ a ~~ b ~~ c ~~ d ~~ e ~~ f'

fmap7 :: Applicative f => (a -> b -> c -> d -> e -> ff -> g -> h) -> f a -> f b -> f c -> f d -> f e -> f ff -> f g -> f h
fmap7 f a b c d e f' g = f <~ a ~~ b ~~ c ~~ d ~~ e ~~ f' ~~ g

fmap8 :: Applicative f => (a -> b -> c -> d -> e -> ff -> g -> h -> i) -> f a -> f b -> f c -> f d -> f e -> f ff -> f g -> f h -> f i
fmap8 f a b c d e f' g h = f <~ a ~~ b ~~ c ~~ d ~~ e ~~ f' ~~ g ~~ h


