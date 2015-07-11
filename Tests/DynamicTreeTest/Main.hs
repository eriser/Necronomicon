import Necronomicon
import GHC.Generics
import Data.Binary
import Data.Foldable (traverse_)

main :: IO ()
main = runSignal megaDark

type Health      = Double
data HeroInput   = HeroKeys (Double, Double) | HeroMouse (Double, Double) | HeroTick (Time, Time) | HeroClick Time | HeroCollision (Time, Collision)
data HeroState   = HeroIdle | HeroMoving Vector3 | HeroAttacking Time | HeroDamaged Time deriving (Show, Eq, Generic)
data Hero        = Hero HeroState Health (Double, Double) deriving (Show, Eq, Generic)
data BulletInput = BulletCollision (Time, [Maybe Collision]) | BulletTick (Time, Time)
data BulletState = Flying Vector3 | DeathAnimation Time deriving (Show, Eq, Generic)
data Bullet      = Bullet BulletState deriving (Show, Eq, Generic)
data PhysM       = EnemyWeapon | HeroWeapon | Player | Neutral deriving (Enum, Show, Eq, Generic)

instance Binary HeroState
instance Binary Hero
instance Binary BulletState
instance Binary Bullet
instance Binary PhysM

mkHero :: Entity Hero
mkHero = ( mkEntity <| Hero HeroIdle 100 (180, 0) )
         { pos        = Vector3 0 0 (-6)
         , rot        = fromEuler 0 180 0
         , collider   = Just <| boxCollider 1 1 1
         , camera     = Just <| Camera 30 0.1 1000 black []
         , netOptions = [NetworkPosition, NetworkRotation] }

mkBullet :: Vector3 -> Entity Bullet
mkBullet p = ( mkEntity <| Bullet <| Flying <| Vector3 1 1 1 )
             { pos      = p
             , collider = Just <| boxCollider 1 1 1
             , model    = Just <| Model cube <| vertexColored white }

initBullets :: Double -> [Entity Bullet]
initBullets offset = foldr (\y acc -> mkBullet (Vector3 0 y 0 + Vector3 offset 0 0) :  acc) [] <| map (*1) [-50..50]

--This is around 10,000 spinning cubes
megaDark :: Signal ()
megaDark = hero *> traverse_ bullets [-200, -196..200]
    where
        hero = foldn updateHero mkHero <| mergeMany
             [ HeroTick      <~ tick
             , HeroKeys      <~ wasd
             , HeroMouse     <~ mouseDelta
             , HeroClick     <~ sampleOn mouseClick runTime
             , HeroCollision <~ timestamp (collision hero) ]

        bullets offset = b
            where
                b = foldn updateBullets (initBullets offset) <| mergeMany
                  [ BulletTick      <~ tick
                  , BulletCollision <~ timestamp (collisionMany b) ]

updateHero :: HeroInput -> Entity Hero -> Entity Hero
updateHero (HeroMouse (mx, my)) h@Entity{ edata = Hero state health (px, py)}
    | HeroIdle     <- state = h'
    | HeroMoving _ <- state = h'
    | otherwise             = h
    where
        x  = floatRem 360   <| px + mx * 80
        y  = clamp (-90) 90 <| py + my * 80
        h' = h{ edata = Hero state health (x, y),
                rot   = fromEuler 0 (-x) 0 * fromEuler (-y) 0 0 }

updateHero (HeroKeys (x, y)) h@Entity{ edata = Hero state health fpr }
    | HeroIdle     <- state = h'
    | HeroMoving _ <- state = h'
    | otherwise             = h
    where
        h' = h{ edata = Hero (HeroMoving <| Vector3 x 0 (-y)) health fpr }

updateHero (HeroTick (dt, rt)) h@Entity{ edata = Hero state health fpr }
    | HeroMoving    p <- state         = translate (p * realToFrac dt * 1.25) h{ edata = Hero state health fpr }
    | HeroAttacking t <- state, rt > t = h{ edata = Hero HeroIdle health fpr }
    | HeroDamaged   t <- state, rt > t = h{ edata = Hero HeroIdle health fpr }
    | otherwise                        = h

updateHero (HeroClick t) h@Entity{ edata = Hero state health fpr }
    | HeroDamaged _ <- state = h
    | otherwise              = h{ edata = Hero (HeroAttacking <| t + 3) health fpr }

updateHero (HeroCollision (t, c)) h@Entity{ edata = Hero _ health fpr }
    | EnemyWeapon <- tag c = h{ edata = Hero (HeroDamaged <| t + 3) (health - 10) fpr }
    | otherwise            = h

updateBullets :: BulletInput -> [Entity Bullet] -> [Entity Bullet]
updateBullets (BulletTick t)            = filterMap' (tickBullet t)
updateBullets (BulletCollision (t, cs)) = map (bulletCollision t) . zip cs

tickBullet :: (Time, Time) -> Entity Bullet -> Maybe (Entity Bullet)
tickBullet (dt, rt) b@Entity{ edata = Bullet state } =
    case state of
        Flying         d -> Just <| rotate (d .*. (dt * 10)) b
        DeathAnimation t -> if rt > t then Nothing else Just b

bulletCollision :: Time -> (Maybe Collision, Entity Bullet) -> Entity Bullet
bulletCollision t (c, b)
    | Nothing          <- mtag = b
    | Just EnemyWeapon <- mtag = b
    | otherwise                = b{ edata = Bullet <| DeathAnimation <| t + 1 }
    where
        mtag = fmap tag c
