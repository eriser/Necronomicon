{-# LANGUAGE Arrows #-}
-- import Necronomicon
import Necronomicon.FRP.SignalA
-- import Control.Arrow

main :: IO ()
main = runSignalM sigLoopTest
-- main = runSignalSS gameS
-- main = runSignalA gameA
-- main = runSignalA $ constant (1 :: Int) >>> state 0 (+)
-- main = runSignal game

-- sigATest :: SignalM Double
-- sigATest = fmap fst $ sigLoopA (0, 0) $ proc (a, b) -> do
--     a' <- sigArr $ (+) <~ fmap fst mousePos -< b
--     b' <- sigArr $ (+) <~ fmap snd mousePos -< a
--     returnA -< (a', b')
--
-- sigLoopTest :: SignalM Double
-- sigLoopTest = sigLoop 0 mouseCounter
--     where
--         mouseCounter = (+) <~ fmap fst mousePos

sigLoopTest :: SignalM Double
sigLoopTest = sigLoop 0 mouseCounter
    where
        mouseCounter = (+) <~ fmap fst mousePos

-- gameA :: SignalA () Int
-- gameA = proc () -> do
--     rec a <- arr (+1)             -< b
--         b <- arr (+1) <<< delay 0 -< a
--     returnA -< b

-- game :: Signal Int
-- game = h
    -- where
        -- h = (+1) <~ delay 0 e
        -- e = (+1) <~ delay 0 h

{-
main = print $ megaDark $ Root []

--This is just a demo
type Health    = Double
data MegaDark  = Hero  Health GameObject
               | Enemy Health GameObject
               | Root  [MegaDark]
               deriving (Show)

--No deriving....time for template haskell?
instance GameType MegaDark where
    _gameObject (Hero  _ g) = g
    _gameObject (Enemy _ g) = g
    _gameObject  _          = gameObject

    gameObject_ g (Hero  h _) = Hero  h g
    gameObject_ g (Enemy h _) = Enemy h g
    gameObject_ _  g          = g

    children (Root c) = c
    children  _       = []

    gchildren_ c (Root _) = Root c
    gchildren_ _  g       = g

mkEnemy :: Vector3 -> MegaDark
mkEnemy    p = Enemy 100 gameObject{
    pos      = p,
    collider = boxCollider 1 1 1
}

mkHero  :: MegaDark
mkHero       = Hero  100 gameObject{
    pos      = Vector3 0 0 10,
    collider = boxCollider 1 2 1,
    camera   = Just $ Camera (60/2) 0.1 1000 black []
}

megaDark :: MegaDark -> MegaDark
megaDark (Root [h, e1, e2, e3, e4]) = Root [h `rotate` Vector3 0.1 0.05 0, e1, e2, e3, e4]
megaDark  _                         = Root [mkHero, mkEnemy 1, mkEnemy 2, mkEnemy (-1), mkEnemy (-2)]
-}
