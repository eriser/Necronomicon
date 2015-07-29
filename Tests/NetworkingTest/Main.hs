import Necronomicon
import GHC.Generics
import Data.Binary
import qualified Data.IntMap as IntMap

main :: IO ()
main = runSignal <| basicNetGUI *> section1

data Player        = Player PlayerState (Double, Double) deriving (Show, Eq, Generic)
data PlayerState   = PlayerIdle
                   | PlayerMoving Vector3
                   deriving (Show, Eq, Generic)
data PlayerInput   = PlayerKeys  (Double, Double) Int
                   | PlayerMouse (Double, Double) Int
                   | PlayerTick  (Time, Time)     Int
                   | PlayerLog   (Int, String)    Int
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
mkPlayer = ( mkEntity  <| Player PlayerIdle (0, 0) )
           { pos        = Vector3 0 0 (-6)
           -- , rot        = fromEuler 0 0 0
           , camera     = Just <| Camera 60 0.1 1000 black [] (toBitMask DefaultLayer) 0
           , netOptions = mkNetworkOptions
               { networkPos    = Network
               , networkRot    = Network
               , networkModel  = NetworkOthers <| Just <| mkModel DefaultLayer cube <| vertexColored white
               , networkCamera = NetworkOthers Nothing } }


mkTerminal :: Vector3 -> Entity Terminal
mkTerminal p = ( mkEntity  <| Terminal (0, 0, 0))
               { pos        = p
               , model      = Just <| mkModel DefaultLayer cube <| ambient <| tga "Gas20.tga"
               , netOptions = mkNetworkOptions
                   { networkPos  = Network
                   , networkData = Network } }

section1 :: Signal ()
section1 = players *> terminals *> pure ()
    where
        players = foldn updatePlayers IntMap.empty
               <| PlayerTick  <~ tick       ~~ userID
               <> PlayerKeys  <~ wasd       ~~ userID
               <> PlayerMouse <~ mouseDelta ~~ userID
               <> PlayerLog   <~ userJoin   ~~ userID

        terminals = foldn updateTerminals [mkTerminal 0]
                 <| TerminalTick <~ tick

updatePlayers :: PlayerInput -> IntMap.IntMap (Entity Player) -> IntMap.IntMap (Entity Player)
updatePlayers input = case input of
    PlayerTick       t uid -> IntMap.adjust (tickPlayer t)        uid
    PlayerKeys       k uid -> IntMap.adjust (playerKeysUpdate k)  uid
    PlayerMouse      m uid -> IntMap.adjust (playerMouseUpdate m) uid
    PlayerLog (pid, _) uid -> if pid == uid then IntMap.insert uid mkPlayer else id

playerMouseUpdate :: (Double, Double) -> Entity Player -> Entity Player
playerMouseUpdate (mx, my) p@Entity{ edata = Player state (px, py) } = p{ edata = Player state (x, y), rot = fromEuler 0 x 0 * fromEuler y 0 0 }
    where
        x  = floatRem 360   <| px + mx * 80
        y  = clamp (-90) 90 <| py + my * 80

playerKeysUpdate :: (Double, Double) -> Entity Player -> Entity Player
playerKeysUpdate (x, y) p@Entity{ edata = Player _ fpr } = p{ edata = Player (PlayerMoving <| Vector3 x 0 y) fpr }

tickPlayer :: (Double, Double) -> Entity Player -> Entity Player
tickPlayer (dt, _) p = case p of
    Entity{ edata = Player (PlayerMoving d) _ } -> translate (d * realToFrac dt * 1.25) p
    _                                           -> p

updateTerminals :: TerminalInput -> [Entity Terminal] -> [Entity Terminal]
updateTerminals (TerminalTick t) = map (tickTerminal t)
updateTerminals _                = id

tickTerminal :: (Time, Time) -> Entity Terminal -> Entity Terminal
tickTerminal (dt, _) t = rotate (realToFrac (dt * 10)) t

