import Necronomicon
import Data.Binary
import GHC.Generics

import qualified Data.ByteString.Lazy as B
import qualified Numeric as N (showHex)

prettyPrint :: B.ByteString -> String
prettyPrint = concat . map (flip N.showHex "") . B.unpack

main :: IO ()
main = do
    let s = (mkHero, [mkBullet]) :: MegaDark
    print s
    putStrLn ""
    let e = encode s
        d = decode e :: MegaDark
    putStrLn $ prettyPrint e
    putStrLn ""
    print d
    putStrLn ""
    print $ show s == show d

type Health = Double
-- type Damage = Double

data HeroState = HeroIdle | HeroMoving Vector3 | HeroAttacking Timer | HeroDamaged Timer deriving (Show, Generic)
data Hero      = Hero HeroState Health deriving (Show, Generic)

data BulletState = Flying Vector3 | DeathAnimation Timer deriving (Show, Generic)
data Bullet      = Bullet BulletState deriving (Show, Generic)

type MegaDark = (Entity Hero, [Entity Bullet])

instance Binary HeroState
instance Binary Hero
instance Binary BulletState
instance Binary Bullet

mkHero :: Entity Hero
mkHero = Entity h g
    where
        h = Hero HeroIdle 100
        g = gameObject{camera = Just c, collider = boxCollider 1 1 1, model = m}
        c = Camera 30 0.1 1000 black [postRenderFX blur]
        m = Just $ Model cube $ vertexColored white

mkBullet :: Entity Bullet
mkBullet = Entity b g
    where
        b = Bullet (Flying 1)
        g = gameObject{collider = boxCollider 1 1 1, model = m}
        m = Just $ Model cube $ vertexColored green

{-
data PhysM = EnemyWeapon
           | HeroWeapon
           | Player
           | Neutral
           deriving (Enum, Show, Generic)


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
-}
