import Necronomicon.FRP.SignalA
import GHC.Generics
import Data.Binary

main :: IO ()
main = runSignal $ sigLoop megaDark initScene

data MegaDark    = MegaDark (Entity Hero) [Entity Bullet] deriving (Show, Eq)
type Health      = Double
data HeroInput   = HeroKeys (Double, Double) | HeroMouse (Double, Double) | HeroTick Time Time | HeroClick Time
data HeroState   = HeroIdle | HeroMoving Vector3 Vector3 | HeroAttacking Time | HeroDamaged Time deriving (Show, Eq, Generic)
data Hero        = Hero HeroState Health deriving (Show, Eq, Generic)
data BulletState = Flying Vector3 | DeathAnimation Time deriving (Show, Eq, Generic)
data Bullet      = Bullet BulletState deriving (Show, Eq, Generic)
data PhysM       = EnemyWeapon
                 | HeroWeapon
                 | Player
                 | Neutral
                 deriving (Enum, Show, Eq, Generic)

instance Binary HeroState
instance Binary Hero
instance Binary BulletState
instance Binary Bullet
instance Binary PhysM

mkHero :: Entity Hero
mkHero = Entity h g
    where
        h = Hero HeroIdle 100
        g = gameObject
          { pos      = Vector3 0 0 (-6)
          , rot      = fromEuler' 0 (-180) 0
          , collider = boxCollider 1 1 1
          , camera   = Just $ Camera 30 0.1 1000 black [] }

mkBullet :: Vector3 -> Entity Bullet
mkBullet p = Entity b g
    where
        b = Bullet (Flying $ Vector3 1 1 1)
        g = gameObject
          { pos      = p
          , collider = boxCollider 1 1 1
          , model    = Just $ Model cube $ vertexColored white }

initScene :: MegaDark
initScene = MegaDark mkHero [mkBullet $ Vector3 (-2) 0 0, mkBullet $ Vector3 0 0 0, mkBullet $ Vector3 2 0 0]

megaDark :: MegaDark -> Signal MegaDark
megaDark (MegaDark hero bullets) = MegaDark <~ heroSig ~~ pure bullets
    where
        heroSig = foldr updateHero hero <~ combine
                [ HeroTick  <~ deltaTime ~~ runTime
                , HeroKeys  <~ wasd
                , HeroMouse <~ mousePos
                , HeroClick <~ sampleOn mouseClick runTime ]

updateHero :: HeroInput -> Entity Hero -> Entity Hero

--directly rotate with mouse?
updateHero (HeroMouse (x, y)) h@(Entity (Hero state health) g)
    | HeroIdle       <- state = Entity (Hero (HeroMoving 0 $ Vector3 y x 0) health) g
    | HeroMoving p _ <- state = Entity (Hero (HeroMoving p $ Vector3 y x 0) health) g
    | otherwise               = h

updateHero (HeroKeys (x, y)) h@(Entity (Hero state health) g)
    | HeroIdle       <- state = Entity (Hero (HeroMoving (Vector3 x 0 (-y)) 0) health) g
    | HeroMoving _ r <- state = Entity (Hero (HeroMoving (Vector3 x 0 (-y)) r) health) g
    | otherwise               = h

updateHero (HeroTick dt rt) h@(Entity (Hero state health) g)
    | HeroMoving  p r <- state = Entity (Hero state health) $ translate (p * realToFrac dt) $ rotate (r * realToFrac dt) g
    | HeroAttacking t <- state
    , t > rt                   = Entity (Hero HeroIdle health) g
    | otherwise                = h

updateHero (HeroClick t) h@(Entity (Hero state health) g)
    | HeroDamaged _ <- state = h
    | otherwise              = Entity (Hero (HeroAttacking t) health) g

--Should fed back values be signals with change event information?
--Is it correct for the feedback to only update when something changes????
--
-- counter :: Signal MegaDark
-- counter = sigLoop sigCount (MegaDark 0 0)
--     where
--         sigCount (MegaDark h e) = MegaDark <~ hsig ~~ esig
--             where
--                 hsig = (+h) <~ fmap fst mousePos
--                 esig = (+e) <~ fmap snd mousePos

{-
import Necronomicon
import Data.Binary
import GHC.Generics

main :: IO ()
main = runGame megaDark initScene

type MegaDark    = (Entity Hero, [Entity Bullet])
type Health      = Double
data HeroState   = HeroIdle | HeroMoving Vector3 Vector3 | HeroAttacking Timer | HeroDamaged Timer deriving (Show, Generic)
data Hero        = Hero HeroState Health deriving (Show, Generic)
data BulletState = Flying Vector3 | DeathAnimation Timer deriving (Show, Generic)
data Bullet      = Bullet BulletState deriving (Show, Generic)
data PhysM       = EnemyWeapon
                 | HeroWeapon
                 | Player
                 | Neutral
                 deriving (Enum, Show, Generic)

instance Binary HeroState
instance Binary Hero
instance Binary BulletState
instance Binary Bullet
instance Binary PhysM

mkHero :: Entity Hero
mkHero = Entity h g
    where
        h = Hero HeroIdle 100
        g = gameObject {
            pos    = Vector3 0 0 (-6),
            rot    = fromEuler' 0 (-180) 0,
            collider = boxCollider 1 1 1,
            camera = Just $ Camera 30 0.1 1000 black []
        }

mkBullet :: Vector3 -> Entity Bullet
mkBullet p = Entity b g
    where
        b = Bullet (Flying $ Vector3 1 1 1)
        g = gameObject {
            pos      = p,
            collider = boxCollider 1 1 1,
            model    = Just $ Model cube $ vertexColored white
        }

initScene :: MegaDark
initScene = (mkHero, [mkBullet $ Vector3 (-2) 0 0, mkBullet $ Vector3 0 0 0, mkBullet $ Vector3 2 0 0])

megaDark :: World -> MegaDark -> MegaDark
megaDark w (hero, bullets) = (updateHero w hero, filterMap (updateBullet w) bullets)

updateHero :: World -> Entity Hero -> Entity Hero
updateHero w (Entity hero g) = Entity hero' (g' hero')
    where
        hero'                        = moveHero . rotateHero . attack . tickHero . foldr checkCollider hero $ collisions g
        g' (Hero (HeroMoving m r) _) = translate (m * realToFrac (deltaTime w * 10)) $ rotate (r * realToFrac (deltaTime w * negate 100)) g
        g'  _                        = g

        moveHero h@(Hero state health)
            | HeroIdle       <- state, Just (x, y) <- moveKeysPressed w = Hero (HeroMoving (Vector3 x 0 (-y)) 0) health
            | HeroMoving _ r <- state, Just (x, y) <- moveKeysPressed w = Hero (HeroMoving (Vector3 x 0 (-y)) r) health
            | otherwise                                                 = h

        rotateHero h@(Hero state health)
            | HeroIdle       <- state, Just (x, y) <- mouseMoved w = Hero (HeroMoving 0 $ Vector3 y x 0) health
            | HeroMoving m _ <- state, Just (x, y) <- mouseMoved w = Hero (HeroMoving m $ Vector3 y x 0) health
            | otherwise                                            = h

        attack h@(Hero state health)
            | HeroIdle       <- state, mouseClicked w = h'
            | HeroMoving _ _ <- state, mouseClicked w = h'
            | otherwise                               = h
            where
                h' = Hero (HeroAttacking $ timer 1 w) health

        tickHero h@(Hero state health)
            | HeroAttacking t <- state, timerReady t w = h'
            | HeroDamaged   t <- state, timerReady t w = h'
            | otherwise                                = h
            where
                h' = Hero HeroIdle health

        checkCollider c h@(Hero _ health)
            | EnemyWeapon <- tag c = Hero (HeroDamaged $ timer 1 w) (health - 10)
            | otherwise            = h

updateBullet :: World -> Entity Bullet -> Maybe (Entity Bullet)
updateBullet w (Entity bullet@(Bullet s) g)
    | DeathAnimation t <- s, timerReady t w = Nothing
    | otherwise                             = Just $ Entity bullet' (g' bullet')
    where
        g' (Bullet (Flying d)) = rotate (d * realToFrac (deltaTime w)) g
        g'  _                  = g
        bullet'                = foldr checkCollider bullet $ collisions g
        checkCollider c b
            | EnemyWeapon <- tag c = b
            | otherwise            = Bullet . DeathAnimation $ timer 0.5 w
-}
