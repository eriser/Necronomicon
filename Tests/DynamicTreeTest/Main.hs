import Necronomicon
import Data.Binary
import GHC.Generics

main :: IO ()
main = runGame megaDark initScene

type MegaDark    = (Entity Hero, [Entity Bullet])
type Health      = Double
data HeroState   = HeroIdle | HeroMoving Vector3 | HeroAttacking Timer | HeroDamaged Timer deriving (Show, Generic)
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
            rot    = fromEuler' 0 180 0,
            camera = Just $ Camera 30 0.1 1000 black []
        }

mkBullet :: Vector3 -> Entity Bullet
mkBullet p = Entity b g
    where
        b = Bullet (Flying $ Vector3 1 1 0)
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
updateHero w (Entity hero g) = Entity hero' (updateGO hero')
    where
        hero'                            = moveHero $ attack $ tickHero $ foldr checkCollider hero $ collisions g
        updateGO (Hero (HeroMoving d) _) = g `rotate` Vector3 (_y d * deltaTime w * negate 100) (_x d * deltaTime w * negate 100) 0
        updateGO  _                      = g

        moveHero h@(Hero state health)
            | HeroIdle     <- state, Just (x, y) <- mouseMoved w = h' x y
            | HeroMoving _ <- state, Just (x, y) <- mouseMoved w = h' x y
            | otherwise                                          = h
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

        checkCollider c h@(Hero _ health)
            | EnemyWeapon <- tag c = Hero (HeroDamaged $ timer 1 w) (health - 10)
            | otherwise            = h

updateBullet :: World -> Entity Bullet -> Maybe (Entity Bullet)
updateBullet w (Entity bullet@(Bullet s) g)
    | DeathAnimation t <- s, timerReady t w = Nothing
    | otherwise                             = Just $ Entity bullet' (moveGO bullet')
    where
        moveGO (Bullet (Flying d)) = g `rotate` (d * realToFrac (runTime w))
        moveGO  _                  = g
        bullet'                    = foldr checkCollider bullet $ collisions g
        checkCollider c b
            | EnemyWeapon <- tag c = b
            | otherwise            = Bullet . DeathAnimation $ timer 0.5 w
