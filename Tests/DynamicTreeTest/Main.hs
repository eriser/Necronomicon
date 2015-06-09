import Necronomicon.FRP.SignalA
import GHC.Generics
import Data.Binary
import Control.Monad (foldM)

--9c68QrEP4v6i

main :: IO ()
main = runSignal megaDark

data MegaDark    = MegaDark (Entity Hero) [Entity Bullet] deriving (Show, Eq, Generic)
type Health      = Double
data HeroInput   = HeroKeys (Double, Double) | HeroMouse (Double, Double) | HeroTick Time Time | HeroClick Time | HeroCollision Time Collision
data HeroState   = HeroIdle | HeroMoving Vector3 | HeroAttacking Time | HeroDamaged Time deriving (Show, Eq, Generic)
data Hero        = Hero HeroState Health deriving (Show, Eq, Generic)
data BulletInput = BulletCollision Time Collision | BulletTick Time Time
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
instance Binary MegaDark
instance Scene  MegaDark

mkHero :: Entity Hero
mkHero = Entity h g
    where
        h = Hero HeroIdle 100
        g = mkGameObject
          { pos      = Vector3 0 0 (-6)
          , rot      = fromEuler 0 0 0
          , collider = boxCollider 1 1 1
          , camera   = Just <| Camera 30 0.1 1000 black [] }

mkBullet :: Vector3 -> Entity Bullet
mkBullet p = Entity b g
    where
        b = Bullet (Flying <| Vector3 1 1 1)
        g = mkGameObject
          { pos      = p
          , collider = boxCollider 1 1 1
          , model    = Just <| Model cube <| vertexColored white }

initBullets :: [Entity Bullet]
initBullets = [mkBullet <| Vector3 (-2) 0 0, mkBullet <| Vector3 0 0 0, mkBullet <| Vector3 2 0 0]

megaDark :: Signal MegaDark
megaDark = MegaDark <~ hero ~~ bullets
    where
        bullets = delay initBullets <| necro <| updateBullets <~ deltaTime ~~ runTime ~~ bullets

        hero    = delay mkHero <| necro <| foldr updateHero <~ hero ~~ hinput
        hinput  = combine
                [ HeroTick  <~ deltaTime ~~ runTime
                , HeroKeys  <~ wasd
                , HeroMouse <~ foldp fpsMouse (180, 0) mouseDelta
                , HeroClick <~ sampleOn mouseClick runTime ]
                <> fmap2 (\h t -> map (HeroCollision t) . collisions <| gameObject h) hero runTime

fpsMouse :: (Double, Double) -> (Double, Double) -> (Double, Double)
fpsMouse (mx, my) (px, py) = (x, y)
    where
        x = floatRem 360   <| px + mx * 80
        y = clamp (-90) 90 <| py + my * 80

updateHero :: HeroInput -> Entity Hero -> Entity Hero
updateHero (HeroMouse (x, y)) h@(Entity (Hero state health) g)
    | HeroIdle     <- state = h'
    | HeroMoving _ <- state = h'
    | otherwise             = h
    where
        h' = Entity (Hero state health) g{rot = fromEuler 0 (-x) 0 * fromEuler (-y) 0 0}

updateHero (HeroKeys (x, y)) h@(Entity (Hero state health) g)
    | HeroIdle     <- state = h'
    | HeroMoving _ <- state = h'
    | otherwise             = h
    where
        h' = Entity (Hero (HeroMoving <| Vector3 x 0 (-y)) health) g

updateHero (HeroTick dt rt) h@(Entity (Hero state health) g)
    | HeroMoving    p <- state         = Entity (Hero state health) <| translate (p * realToFrac dt * 1.25) g
    | HeroAttacking t <- state, rt > t = Entity (Hero HeroIdle health) g
    | HeroDamaged   t <- state, rt > t = Entity (Hero HeroIdle health) g
    | otherwise                        = h

updateHero (HeroClick t) h@(Entity (Hero state health) g)
    | HeroDamaged _ <- state = h
    | otherwise              = Entity (Hero (HeroAttacking <| t + 3) health) g

updateHero (HeroCollision t c) h@(Entity (Hero _ health) g)
    | EnemyWeapon <- tag c = Entity (Hero (HeroDamaged <| t + 3) (health - 10)) g
    | otherwise            = h


updateBullets :: Time -> Time -> [Entity Bullet] -> [Entity Bullet]
updateBullets rt dt bs = filterMap updateM bs
    where
        updateM b = foldM updateBullet b <| BulletTick rt dt : map (BulletCollision rt) (collisions <| gameObject b)

updateBullet :: Entity Bullet -> BulletInput -> Maybe (Entity Bullet)
updateBullet b@(Entity (Bullet state) g) (BulletTick dt rt)
    | DeathAnimation t <- state, rt > t = Nothing
    | Flying         d <- state         = Just <| Entity (Bullet state) <| rotate (d * realToFrac dt * 10) g
    | otherwise                         = Just b

updateBullet b@(Entity _ g) (BulletCollision t c)
    | EnemyWeapon <- tag c = Just <| b
    | otherwise            = Just <| Entity (Bullet <| DeathAnimation <| t + 1) g

{-
main :: IO ()
main = runGameSignal megaDark initScene

data MegaDark    = MegaDark { hero :: Entity Hero, bullets :: [Entity Bullet] } deriving (Show, Eq, Generic)
type Health      = Double
data HeroInput   = HeroKeys (Double, Double) | HeroMouse (Double, Double) | HeroTick Time Time | HeroClick Time | HeroCollision Time Collision
data HeroState   = HeroIdle | HeroMoving Vector3 | HeroAttacking Time | HeroDamaged Time deriving (Show, Eq, Generic)
data Hero        = Hero HeroState Health deriving (Show, Eq, Generic)
data BulletInput = BulletCollision Time Collision | BulletTick Time Time
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
instance Binary MegaDark
instance Scene  MegaDark

mkHero :: Entity Hero
mkHero = Entity h g
    where
        h = Hero HeroIdle 100
        g = gameObject
          { pos      = Vector3 0 0 (-6)
          , rot      = fromEuler 0 0 0
          , collider = boxCollider 1 1 1
          , camera   = Just <| Camera 30 0.1 1000 black [] }

mkBullet :: Vector3 -> Entity Bullet
mkBullet p = Entity b g
    where
        b = Bullet (Flying <| Vector3 1 1 1)
        g = gameObject
          { pos      = p
          , collider = boxCollider 1 1 1
          , model    = Just <| Model cube <| vertexColored white }

initScene :: MegaDark
initScene = MegaDark mkHero [mkBullet <| Vector3 (-2) 0 0, mkBullet <| Vector3 0 0 0, mkBullet <| Vector3 2 0 0]

megaDark :: Signal MegaDark -> Signal MegaDark
megaDark mega = MegaDark <~ heroSig ~~ bSig
    where
        bSig    = updateBullets <~ deltaTime ~~ runTime ~~ fmap bullets mega

        heroSig = foldr updateHero <~ fmap hero mega ~~ hInput
        hcs     = map <~ fmap HeroCollision runTime ~~ fmap (collisions . gameObject . hero) mega
        hInput  = (++) <~ hcs ~~ combine
                [ HeroTick  <~ deltaTime ~~ runTime
                , HeroKeys  <~ wasd
                , HeroMouse <~ foldp fpsMouse (180, 0) mousePosR
                , HeroClick <~ sampleOn mouseClick runTime ]

-- megaDark :: Signal MegaDark
-- megaDark = MegaDark <~ present hs ~~ present bs
--     where
--         bs = stream mkBullets <| updateBullets <~ past bs ~~ ((,) <~ deltaTime ~~ runTime)
--         hs = stream mkHero    <| updateHero    <~ present hs ~~ merge
--                 [ HeroTick      <~ deltaTime ~~ runTime
--                 , HeroKeys      <~ wasd
--                 , HeroMouse     <~ foldp fpsMouse (180, 0) mousePosR
--                 , HeroClick     <~ sampleOn mouseClick runTime
--                 , HeroCollision <~ collisions (past hs) ]

fpsMouse :: (Double, Double) -> (Double, Double) -> (Double, Double)
fpsMouse (mx, my) (px, py) = (x, y)
    where
        x = floatRem 360   <| px + mx * 80
        y = clamp (-90) 90 <| py + my * 80

updateHero :: HeroInput -> Entity Hero -> Entity Hero
updateHero (HeroMouse (x, y)) h@(Entity (Hero state health) g)
    | HeroIdle     <- state = h'
    | HeroMoving _ <- state = h'
    | otherwise             = h
    where
        h' = Entity (Hero state health) g{rot = fromEuler 0 (-x) 0 * fromEuler (-y) 0 0}

updateHero (HeroKeys (x, y)) h@(Entity (Hero state health) g)
    | HeroIdle     <- state = h'
    | HeroMoving _ <- state = h'
    | otherwise             = h
    where
        h' = Entity (Hero (HeroMoving <| Vector3 x 0 (-y)) health) g

updateHero (HeroTick dt rt) h@(Entity (Hero state health) g)
    | HeroMoving    p <- state         = Entity (Hero state health) <| translate (p * realToFrac dt * 1.25) g
    | HeroAttacking t <- state, rt > t = Entity (Hero HeroIdle health) g
    | HeroDamaged   t <- state, rt > t = Entity (Hero HeroIdle health) g
    | otherwise                        = h

updateHero (HeroClick t) h@(Entity (Hero state health) g)
    | HeroDamaged _ <- state = h
    | otherwise              = Entity (Hero (HeroAttacking <| t + 3) health) g

updateHero (HeroCollision t c) h@(Entity (Hero _ health) g)
    | EnemyWeapon <- tag c = Entity (Hero (HeroDamaged <| t + 3) (health - 10)) g
    | otherwise            = h



updateBullets :: Time -> Time -> [Entity Bullet] -> [Entity Bullet]
updateBullets rt dt bs = filterMap updateM bs
    where
        updateM b = foldM updateBullet b <| BulletTick rt dt : map (BulletCollision rt) (collisions <| gameObject b)

updateBullet :: Entity Bullet -> BulletInput -> Maybe (Entity Bullet)
updateBullet b@(Entity (Bullet state) g) (BulletTick dt rt)
    | DeathAnimation t <- state, rt > t = Nothing
    | Flying         d <- state         = Just <| Entity (Bullet state) <| rotate (d * realToFrac dt * 10) g
    | otherwise                         = Just b

updateBullet b@(Entity _ g) (BulletCollision t c)
    | EnemyWeapon <- tag c = Just <| b
    | otherwise            = Just <| Entity (Bullet <| DeathAnimation <| t + 1) g
-}


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
            camera = Just <| Camera 30 0.1 1000 black []
        }

mkBullet :: Vector3 -> Entity Bullet
mkBullet p = Entity b g
    where
        b = Bullet (Flying <| Vector3 1 1 1)
        g = gameObject {
            pos      = p,
            collider = boxCollider 1 1 1,
            model    = Just <| Model cube <| vertexColored white
        }

initScene :: MegaDark
initScene = (mkHero, [mkBullet <| Vector3 (-2) 0 0, mkBullet <| Vector3 0 0 0, mkBullet <| Vector3 2 0 0])

megaDark :: World -> MegaDark -> MegaDark
megaDark w (hero, bullets) = (updateHero w hero, filterMap (updateBullet w) bullets)

updateHero :: World -> Entity Hero -> Entity Hero
updateHero w (Entity hero g) = Entity hero' (g' hero')
    where
        hero'                        = moveHero . rotateHero . attack . tickHero . foldr checkCollider hero <| collisions g
        g' (Hero (HeroMoving m r) _) = translate (m * realToFrac (deltaTime w * 10)) <| rotate (r * realToFrac (deltaTime w * negate 100)) g
        g'  _                        = g

        moveHero h@(Hero state health)
            | HeroIdle       <- state, Just (x, y) <- moveKeysPressed w = Hero (HeroMoving (Vector3 x 0 (-y)) 0) health
            | HeroMoving _ r <- state, Just (x, y) <- moveKeysPressed w = Hero (HeroMoving (Vector3 x 0 (-y)) r) health
            | otherwise                                                 = h

        rotateHero h@(Hero state health)
            | HeroIdle       <- state, Just (x, y) <- mouseMoved w = Hero (HeroMoving 0 <| Vector3 y x 0) health
            | HeroMoving m _ <- state, Just (x, y) <- mouseMoved w = Hero (HeroMoving m <| Vector3 y x 0) health
            | otherwise                                            = h

        attack h@(Hero state health)
            | HeroIdle       <- state, mouseClicked w = h'
            | HeroMoving _ _ <- state, mouseClicked w = h'
            | otherwise                               = h
            where
                h' = Hero (HeroAttacking <| timer 1 w) health

        tickHero h@(Hero state health)
            | HeroAttacking t <- state, timerReady t w = h'
            | HeroDamaged   t <- state, timerReady t w = h'
            | otherwise                                = h
            where
                h' = Hero HeroIdle health

        checkCollider c h@(Hero _ health)
            | EnemyWeapon <- tag c = Hero (HeroDamaged <| timer 1 w) (health - 10)
            | otherwise            = h

updateBullet :: World -> Entity Bullet -> Maybe (Entity Bullet)
updateBullet w (Entity bullet@(Bullet s) g)
    | DeathAnimation t <- s, timerReady t w = Nothing
    | otherwise                             = Just <| Entity bullet' (g' bullet')
    where
        g' (Bullet (Flying d)) = rotate (d * realToFrac (deltaTime w)) g
        g'  _                  = g
        bullet'                = foldr checkCollider bullet <| collisions g
        checkCollider c b
            | EnemyWeapon <- tag c = b
            | otherwise            = Bullet . DeathAnimation <| timer 0.5 w
-}
