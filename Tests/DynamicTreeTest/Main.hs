-- {-# LANGUAGE Arrows #-}
import Necronomicon
-- import Necronomicon.FRP.SignalA
-- import Control.Arrow

main :: IO ()
main = print "test"

type Health = Double
type Damage = Double

data HeroState = HeroIdle | HeroMoving Vector3 | HeroAttacking Timer | HeroDamaged Timer
data Hero      = Hero HeroState Health

data BulletState = Flying Vector3 | DeathAnimation Timer
data Bullet      = Bullet BulletState

data PhysM = EnemyWeapon
           | HeroWeapon
           | Player
           | Neutral
           deriving (Enum)

type MegaDark = (Entity Hero, [Entity Bullet])

megaDark :: World -> MegaDark -> MegaDark
megaDark w (hero, bullets) = (updateHero w hero, mapCollapse (updateBullet w) bullets)

updateBullet :: World -> Entity Bullet -> Maybe (Entity Bullet)
updateBullet w (Entity bullet@(Bullet s) g)
    | DeathAnimation t <- s, timerReady t w = Nothing
    | otherwise                             = Just $ Entity bullet' (moveGO bullet')
    where
        moveGO (Bullet (Flying d)) = move g d
        bullet'                    = foldr checkCollider bullet $ collisions g
        checkCollider c b
            | EnemyWeapon <- tag c = b
            | otherwise            = Bullet . DeathAnimation $ timer 0.5 w

updateHero :: World -> Entity Hero -> Entity Hero
updateHero w (Entity hero g) = Entity hero' (updateGO hero')
    where
        hero'                            = moveHero $ attack $ tickHero $ foldr checkCollider hero $ collisions g
        updateGO (Hero (HeroMoving d) _) = move g d
        updateGO  _                      = g

        moveHero h@(Hero state health)
            | HeroIdle     <- state, Just (x, y) <- moveKeysPressed w = h' x y
            | HeroMoving _ <- state, Just (x, y) <- moveKeysPressed w = h' x y
            | otherwise                                               = h
            where
                h' x y = Hero (HeroMoving $ Vector3 x y 0) health

        attack h@(Hero state health)
            | HeroIdle     <- state, mouseClicked w = h'
            | HeroMoving _ <- state, mouseClicked w = h'
            | otherwise                             = h
            where
                h' = Hero (HeroAttacking $ timer 1 w) health

        tickHero h@(Hero state health)
            | HeroAttacking t <- state, timerReady t w = h'
            | HeroDamaged   t <- state, timerReady t w = h'
            | otherwise                                = h
            where
                h' = Hero HeroIdle health

        checkCollider c h@(Hero state health)
            | EnemyWeapon <- tag c = Hero (HeroDamaged $ timer 1 w) (health - 10)
            | otherwise            = h

-- main = runSignalM sigLoopTest
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
--
-- sigLoopTest :: SignalM Double
-- sigLoopTest = mouseCounter
    -- where
        -- mouseCounter = (+) <~ fmap fst mousePos ~~ delayM 0 mouseCounter
--
-- sigLoopTest :: SignalM Double
-- sigLoopTest = fmap fst $ sigLoop (0, 0) scene
--     where
--         scene = (\hf ef (h', e') -> (hf e', ef h')) <~ hero ~~ enemy
--         hero  = (\h e -> h + e) <~ pure 1
--         enemy = (\e h -> h + e) <~ pure 1

-- sigLoopTest :: SignalM Double
-- sigLoopTest = foldp (\h m e -> h + m + e) 0 (fmap fst mousePos) ~~ pure 2
    -- where
    --     scene = (\hf ef (h', e') -> (hf e', ef h')) <~ hero ~~ enemy
    --     hero  = foldp (\h e -> h + e + 1) 0 (pure 0)
    --     enemy = foldp (\e h -> h + h + 1) 0 (pure 0)

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
