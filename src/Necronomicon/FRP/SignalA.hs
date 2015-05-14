module Necronomicon.FRP.SignalA where

------------------------------------------------------
-- import           Control.Applicative
import           Control.Arrow hiding (second)
import qualified Control.Category as Cat
------------------------------------------------------

(<~) :: Functor f => (a -> b) -> f a -> f b
(<~) = fmap

(~~) :: Applicative f => f (a -> b) -> f a -> f b
(~~) = (<*>)

(~>) :: Functor f => f a -> (a -> b) -> f b
(~>) = flip fmap

infixl 4 <~,~~
infixr 4 ~>

newtype Time = Time Double

-------------------------------------------------------
-- AFRP
-------------------------------------------------------
--Live coding?

-- type Time       = Double
data Signal a b = SignalGen   (Time -> a -> (Signal a b, b))          --Normal Signal, depends on time and input sample
                | SignalArr   (Time -> a -> (Signal a b, b)) (a -> b) --Lifted pure function, depends purely on input sample
                | SignalConst (Time -> a -> (Signal a b, b))  b       --Constant Signal, not dependent on time or input sample

runS :: Signal a b -> (Time -> a -> (Signal a b, b))
runS (SignalGen   f)   = f
runS (SignalArr   f _) = f
runS (SignalConst f _) = f

constant :: b -> Signal a b
constant b = sig
    where
        sig       = SignalConst cont b
        cont _ _ = (sig, b)

instance Cat.Category Signal where
    id                                = SignalArr (\_ x -> (Cat.id, x)) id
    SignalConst _ c . _               = constant c
    SignalArr   _ f . SignalConst _ c = constant (f c)
    SignalArr   _ f . SignalArr   _ g = arr (f . g)
    sb . sa                           = SignalGen cont
        where
            cont dt x = (sbCont Cat.. saCont, x2)
                where
                    (saCont, x1) = runS sa dt x
                    (sbCont, x2) = runS sb dt x1

instance Arrow Signal where
    arr f = sig
        where
            sig      = SignalArr cont f
            cont _ a = (sig, f a)

    first (SignalConst _ c) = sig
        where
            sig           = SignalArr cont (\(_,d) -> (c, d))
            cont _ (_, d) = (sig, (c, d))
    first (SignalArr _ f) = sig
        where
            sig           = SignalArr cont (\(b, d) -> (f b, d))
            cont _ (b, d) = (sig, (f b, d))
    first (SignalGen cont1) = sig
        where
            sig           = SignalGen cont
            cont t (b, d) = (first cont2, (c, d))
                where
                    (cont2, c) = cont1 t b

-- instance ArrowApply Signal where
    -- app =

--foldp or state???
--maybe define this in terms of arrow loop instead!??!?!
--I think this is where the space leak would exhibit itself
state :: s -> (a -> s -> s) -> Signal a s
state s f = SignalGen $ \_ a -> let x = f a s in (state x f, x)

foldp :: (a -> s -> s) -> s -> Signal a s
foldp = flip state

plus1 :: Signal Int Int
plus1 = arr (+1)

plus2 :: Signal Int Int
plus2 = plus1 >>> plus1

instance Functor (Signal a) where
    fmap f a = a >>> arr f -- Maybe implement this for better efficiency

instance Applicative (Signal a) where
    pure      = constant
    af <*> ax = fmap (uncurry ($)) (af &&& ax) -- Maybe implement this for better efficiency

test1 :: Signal () Int
test1 = constant 1

test2 :: Signal () Int
test2 = (+) <~ constant 1 ~~ constant 2

test :: Signal () (Int -> Int)
test = (+) <~ constant 1

data HeroA = HeroA Double

test4 :: Signal () HeroA
test4 = foldp (\dmg (HeroA h) -> HeroA (h - dmg)) (HeroA 100 ) <<< constant 3000

--try a test with an open loop feedback for game entities
--maybe need delay combinator

instance ArrowLoop Signal where
    loop (SignalConst _ c) = constant (fst c)
    loop  sig              = SignalGen $ \dt b -> let (sig', (c, d)) = runS sig dt (b, d) in (loop sig', c)
