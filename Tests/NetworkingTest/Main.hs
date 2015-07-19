import Necronomicon
import GHC.Generics
import Data.Binary

main :: IO ()
main = runSignal section1

data Player        = Player PlayerState (Double, Double) deriving (Show, Eq, Generic)
data PlayerState   = PlayerIdle | PlayerMoving Vector3 deriving (Show, Eq, Generic)
data PlayerInput   = PlayerKeys (Double, Double) | PlayerMouse (Double, Double) | PlayerTick (Time, Time) deriving (Show, Eq, Generic)

data Terminal      = Terminal (Double, Double, Double) deriving (Show, Eq, Generic)
data TerminalInput = TerminalTick (Time, Time) | TerminalAdd | TerminalRemove deriving (Show, Eq, Generic)

instance Binary Player
instance Binary PlayerState
instance Binary Terminal

mkPlayer :: Entity Player
mkPlayer = ( mkEntity <| Player PlayerIdle (180, 0) )
           { pos        = Vector3 0 0 (-6)
           , rot        = fromEuler 0 180 0
           , camera     = Just <| Camera 30 0.1 1000 black []
           , netOptions = [NetworkPosition, NetworkRotation] }
   
mkTerminal :: Vector3 -> Entity Terminal
mkTerminal p = ( mkEntity <| Terminal (0, 0, 0))
             { pos      = p
             , model    = Just <| Model cube <| vertexColored white
             , netOptions = [NetworkPosition] }

section1 :: Signal ()
section1 = players *> terminals *> pure ()
    where
        players = foldn updatePlayers mkPlayer <| mergeMany
                [ PlayerTick      <~ tick
                , PlayerKeys      <~ wasd
                , PlayerMouse     <~ mouseDelta ]
           
        terminals = foldn updateTerminals [mkTerminal 0] <| mergeMany
                  [ TerminalTick      <~ tick ]

updatePlayers :: PlayerInput -> Entity Player -> Entity Player
updatePlayers = updatePlayer

updatePlayer :: PlayerInput -> Entity Player -> Entity Player
updatePlayer (PlayerMouse (mx, my)) p@Entity{ edata = Player state (px, py)}
    | PlayerIdle     <- state = p'
    | PlayerMoving _ <- state = p'
    | otherwise               = p
    where
        x  = floatRem 360   <| px + mx * 80
        y  = clamp (-90) 90 <| py + my * 80
        p' = p{ edata = Player state (x, y),
                rot   = fromEuler 0 (-x) 0 * fromEuler (-y) 0 0 }

updatePlayer (PlayerKeys (x, y)) p@Entity{ edata = Player state fpr }
    | PlayerIdle     <- state = p'
    | PlayerMoving _ <- state = p'
    | otherwise               = p
    where
        p' = p{ edata = Player (PlayerMoving <| Vector3 x 0 (-y)) fpr }

updatePlayer (PlayerTick (dt, _)) p@Entity{ edata = Player state fpr }
    | PlayerMoving    d <- state         = translate (d * realToFrac dt * 1.25) p{ edata = Player state fpr }
    | otherwise                          = p

updateTerminals :: TerminalInput -> [Entity Terminal] -> [Entity Terminal]
updateTerminals (TerminalTick t) = map (tickTerminal t)
updateTerminals _                = id

tickTerminal :: (Time, Time) -> Entity Terminal -> Entity Terminal
tickTerminal (dt, _) t = rotate (realToFrac (dt * 10)) t

