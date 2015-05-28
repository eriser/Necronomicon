import Necronomicon.FRP.SignalA
import Control.Applicative

main :: IO ()
main = runSignal $ mousePos <|> mousePos <|> mousePos
-- main = runSignal counter
--
-- counter :: Signal (Double, Double)
-- counter = sigLoop sigCount (0, 0)
--     where
--         sigCount s = (,) <~ hsig ~~ esig
--             where
--                 hsig = pureCount (fst s) <~ fmap fst mousePos
--                 esig = pureCount (snd s) <~ fmap snd mousePos
--         pureCount x y = x + y

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
megaDark w (hero, bullets) = (updateHero w hero, mapCollapse (updateBullet w) bullets)

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
