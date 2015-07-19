import Necronomicon
import GHC.Generics
import Data.Binary
import qualified Data.IntMap as IntMap

main :: IO ()
main = runSignal section1

data Player        = Player PlayerState (Double, Double) deriving (Show, Eq, Generic)
data PlayerState   = PlayerIdle
                   | PlayerMoving Vector3
                   deriving (Show, Eq, Generic)
data PlayerInput   = PlayerKeys  (Double, Double)    Int
                   | PlayerMouse (Double, Double)    Int
                   | PlayerTick  (Time, Time)        Int
                   | PlayerLog   (Int, String, Bool) Int
                   deriving (Show, Eq, Generic)

data Terminal      = Terminal (Double, Double, Double) deriving (Show, Eq, Generic)
data TerminalInput = TerminalTick (Time, Time)
                   | TerminalAdd
                   | TerminalRemove
                   deriving (Show, Eq, Generic)

instance Binary Player
instance Binary PlayerState
instance Binary Terminal

mkPlayer :: Entity Player
mkPlayer = ( mkEntity  <| Player PlayerIdle (180, 0) )
           { pos        = Vector3 0 0 (-6)
           , rot        = fromEuler 0 180 0
           , camera     = Just <| Camera 30 0.1 1000 black []
           , netOptions = [NetworkPosition, NetworkRotation] }
   
mkTerminal :: Vector3 -> Entity Terminal
mkTerminal p = ( mkEntity  <| Terminal (0, 0, 0))
               { pos        = p
               , model      = Just <| Model cube <| vertexColored white
               , netOptions = [NetworkPosition] }
   
section1 :: Signal ()
section1 = players *> terminals *> pure ()
    where
        players = foldn updatePlayers IntMap.empty <| mergeMany
                [ PlayerTick  <~ tick       ~~ userID
                , PlayerKeys  <~ wasd       ~~ userID
                , PlayerMouse <~ mouseDelta ~~ userID
                , PlayerLog   <~ userLog    ~~ userID ]
           
        terminals = foldn updateTerminals [mkTerminal 0] <| mergeMany
                  [ TerminalTick <~ tick ]

updatePlayers :: PlayerInput -> IntMap.IntMap (Entity Player) -> IntMap.IntMap (Entity Player)
updatePlayers (PlayerTick          t uid) ps = IntMap.adjust (tickPlayer t)        uid ps
updatePlayers (PlayerKeys          k uid) ps = IntMap.adjust (playerKeysUpdate k)  uid ps
updatePlayers (PlayerMouse         m uid) ps = IntMap.adjust (playerMouseUpdate m) uid ps
updatePlayers (PlayerLog (pid, _, i) uid) ps
    | pid == uid && i = IntMap.insert uid mkPlayer ps 
    | pid == uid      = IntMap.delete uid ps
    | otherwise       = ps

playerMouseUpdate :: (Double, Double) -> Entity Player -> Entity Player
playerMouseUpdate (mx, my) p@Entity{ edata = Player state (px, py)} = case state of
    PlayerIdle     -> p'
    PlayerMoving _ -> p'
    where
        x  = floatRem 360   <| px + mx * 80
        y  = clamp (-90) 90 <| py + my * 80
        p' = p{ edata = Player state (x, y),
                rot   = fromEuler 0 (-x) 0 * fromEuler (-y) 0 0 }

playerKeysUpdate :: (Double, Double) -> Entity Player -> Entity Player
playerKeysUpdate (x, y) p@Entity{ edata = Player state fpr } = case state of
    PlayerIdle     -> p'
    PlayerMoving _ -> p'
    where
        p' = p{ edata = Player (PlayerMoving <| Vector3 x 0 (-y)) fpr }

tickPlayer :: (Double, Double) -> Entity Player -> Entity Player
tickPlayer (dt, _) p@Entity{ edata = Player state fpr } = case state of
    PlayerMoving    d -> translate (d * realToFrac dt * 1.25) p{ edata = Player state fpr }
    _                 -> p

updateTerminals :: TerminalInput -> [Entity Terminal] -> [Entity Terminal]
updateTerminals (TerminalTick t) = map (tickTerminal t)
updateTerminals _                = id

tickTerminal :: (Time, Time) -> Entity Terminal -> Entity Terminal
tickTerminal (dt, _) t = rotate (realToFrac (dt * 10)) t

