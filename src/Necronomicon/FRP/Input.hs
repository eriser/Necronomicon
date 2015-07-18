module Necronomicon.FRP.Input
    ( dimensions
    , mousePos
    , mouseDelta
    , mouseX
    , mouseY
    , mouseButton
    , mouseClick
    , keys
    , keyA
    , keyB
    , keyC
    , keyD
    , keyE
    , keyF
    , keyG
    , keyH
    , keyI
    , keyJ
    , keyK
    , keyL
    , keyM
    , keyN
    , keyO
    , keyP
    , keyQ
    , keyR
    , keyS
    , keyT
    , keyU
    , keyV
    , keyW
    , keyX
    , keyY
    , keyZ
    , keyEnter
    , keyLCtrl
    , keyRCtrl
    , keyLAlt
    , keyRAlt
    , keySpace
    , keyLShift
    , keyRShift
    , keyBackspace
    , key0
    , key1
    , key2
    , key3
    , key4
    , key5
    , key6
    , key7
    , key8
    , key9
    , keyApostrophe
    , keyComma
    , keyMinus
    , keyEqual
    , keyPeriod
    , keySlash
    , keySemiColon
    , keyLeftBracket
    , keyBackSlash
    , keyRightBracket
    , keyGraveAccent
    , keyUp
    , keyDown
    , keyLeft
    , keyRight
    , isDown
    , isUp
    , wasd
    , keyboard
    , collision
--    , collisionMany
    ) where

import           Necronomicon.FRP.Types
import           Necronomicon.FRP.Signal
import           Necronomicon.Physics
--import           Necronomicon.Entity
import           Necronomicon.FRP.Runtime
--import           Necronomicon.FRP.State
import           Necronomicon.Graphics.Resources (UID(..))

import           Data.IORef
import qualified Data.IntSet                  as IntSet
import qualified Data.IntMap                  as IntMap
import qualified Data.Map                     as Map
import qualified Graphics.UI.GLFW             as GLFW

----------------------------------
-- Input
----------------------------------

mousePos :: Signal (Double, Double)
mousePos = inputSignal 201 mousePosRef

mouseDelta :: Signal (Double, Double)
mouseDelta = Signal $ \state -> do
    GLFW.setCursorInputMode     (context state) GLFW.CursorInputMode'Disabled
    let mref   = mousePosRef   state
    ref       <- newIORef ((0, 0), (0, 0))
    return (cont ref mref, (0, 0), IntSet.singleton 201)
    where
        cont ref mref eid
            | eid /= 201 = readIORef ref >>= return . NoChange . snd
            | otherwise  =  do
                (mx, my) <- readIORef mref
                (px, py) <- fst <~ readIORef ref
                let delta = ((mx - px), (my - py))
                writeIORef ref ((mx, my), delta)
                return $ Change delta

mouseClick :: Signal ()
mouseClick = Signal $ \state -> do
    let iref = mouseClickRef state
    return (cont iref, (), IntSet.singleton 202)
    where
        cont iref eid
            | eid /= 202 = return $ NoChange ()
            | otherwise  = readIORef iref >>= \b -> if not b
                then return $ NoChange ()
                else return $ Change   ()

mouseX :: Signal Double
mouseX = fst <~ mousePos

mouseY :: Signal Double
mouseY = snd <~ mousePos

mouseButton :: Signal Bool
mouseButton = inputSignal 202 mouseClickRef

dimensions :: Signal (Double, Double)
dimensions = inputSignal 203 dimensionsRef

keyA :: GLFW.Key
keyA = GLFW.Key'A

keyB :: GLFW.Key
keyB = GLFW.Key'B

keyC :: GLFW.Key
keyC = GLFW.Key'C

keyD :: GLFW.Key
keyD = GLFW.Key'D

keyE :: GLFW.Key
keyE = GLFW.Key'E

keyF :: GLFW.Key
keyF = GLFW.Key'F

keyG :: GLFW.Key
keyG = GLFW.Key'G

keyH :: GLFW.Key
keyH = GLFW.Key'H

keyI :: GLFW.Key
keyI = GLFW.Key'I

keyJ :: GLFW.Key
keyJ = GLFW.Key'J

keyK :: GLFW.Key
keyK = GLFW.Key'K

keyL :: GLFW.Key
keyL = GLFW.Key'L

keyM :: GLFW.Key
keyM = GLFW.Key'M

keyN :: GLFW.Key
keyN = GLFW.Key'N

keyO :: GLFW.Key
keyO = GLFW.Key'O

keyP :: GLFW.Key
keyP = GLFW.Key'P

keyQ :: GLFW.Key
keyQ = GLFW.Key'Q

keyR :: GLFW.Key
keyR = GLFW.Key'R

keyS :: GLFW.Key
keyS = GLFW.Key'S

keyT :: GLFW.Key
keyT = GLFW.Key'T

keyU :: GLFW.Key
keyU = GLFW.Key'U

keyV :: GLFW.Key
keyV = GLFW.Key'V

keyW :: GLFW.Key
keyW = GLFW.Key'W

keyX :: GLFW.Key
keyX = GLFW.Key'X

keyY :: GLFW.Key
keyY = GLFW.Key'Y

keyZ :: GLFW.Key
keyZ = GLFW.Key'Z

keyEnter :: GLFW.Key
keyEnter  = GLFW.Key'Enter

keyLCtrl :: GLFW.Key
keyLCtrl  = GLFW.Key'LeftControl

keyRCtrl :: GLFW.Key
keyRCtrl  = GLFW.Key'RightControl

keyLAlt :: GLFW.Key
keyLAlt   = GLFW.Key'LeftAlt

keyRAlt :: GLFW.Key
keyRAlt   = GLFW.Key'RightAlt

keySpace :: GLFW.Key
keySpace  = GLFW.Key'Space

keyLShift :: GLFW.Key
keyLShift = GLFW.Key'LeftShift

keyRShift :: GLFW.Key
keyRShift = GLFW.Key'RightShift

keyBackspace :: GLFW.Key
keyBackspace = GLFW.Key'Backspace

key0 :: GLFW.Key
key0 = GLFW.Key'0

key1 :: GLFW.Key
key1 = GLFW.Key'1

key2 :: GLFW.Key
key2 = GLFW.Key'2

key3 :: GLFW.Key
key3 = GLFW.Key'3

key4 :: GLFW.Key
key4 = GLFW.Key'4

key5 :: GLFW.Key
key5 = GLFW.Key'5

key6 :: GLFW.Key
key6 = GLFW.Key'6

key7 :: GLFW.Key
key7 = GLFW.Key'7

key8 :: GLFW.Key
key8 = GLFW.Key'8

key9 :: GLFW.Key
key9 = GLFW.Key'9

keyApostrophe :: GLFW.Key
keyApostrophe = GLFW.Key'Apostrophe

keyComma :: GLFW.Key
keyComma = GLFW.Key'Comma

keyMinus :: GLFW.Key
keyMinus = GLFW.Key'Minus

keyEqual :: GLFW.Key
keyEqual = GLFW.Key'Equal

keyPeriod :: GLFW.Key
keyPeriod = GLFW.Key'Period

keySlash :: GLFW.Key
keySlash = GLFW.Key'Slash

keySemiColon :: GLFW.Key
keySemiColon = GLFW.Key'Semicolon

keyLeftBracket :: GLFW.Key
keyLeftBracket = GLFW.Key'LeftBracket

keyBackSlash :: GLFW.Key
keyBackSlash = GLFW.Key'Backslash

keyRightBracket :: GLFW.Key
keyRightBracket = GLFW.Key'RightBracket

keyGraveAccent :: GLFW.Key
keyGraveAccent = GLFW.Key'GraveAccent

keyUp :: GLFW.Key
keyUp    = GLFW.Key'Up

keyDown :: GLFW.Key
keyDown  = GLFW.Key'Down

keyLeft :: GLFW.Key
keyLeft  = GLFW.Key'Left

keyRight :: GLFW.Key
keyRight = GLFW.Key'Right

-- map (\k -> (fromEnum k, NoChange False))
--     [keyA, keyB, keyC, keyD, keyE, keyF, keyG, keyH, keyI, keyJ, keyK, keyL, keyM
--     ,keyN, keyO, keyP, keyQ, keyR, keyS, keyT, keyU, keyV, keyW, keyX, keyY, keyZ
--     ,keyEnter, keySpace, keyLCtrl, keyRCtrl, keyLAlt, keyRAlt, keySpace, keyLShift
--     ,keyRShift, keyBackspace, key0, key1, key2, key3, key4, key5, key6, key7, key8
--     ,key9, keyApostrophe, keyComma, keyMinus, keyEqual, keyPeriod, keySlash, keySemiColon
--     ,keyLeftBracket, keyBackSlash, keyRightBracket, keyGraveAccent, keyUp, keyDown
--     ,keyLeft, keyRight]

isUp :: Key -> Signal Bool
isUp k = not <~ isDown k

isDown :: Key -> Signal Bool
isDown k = Signal $ \state -> do
    ref <- newIORef False
    return (cont (keyboardRef state) ref (fromEnum k), False, IntSet.singleton (fromEnum k))
    where
        cont ksref ref uid eid
            | eid /= uid = readIORef ref >>= return . NoChange
            | otherwise  = do
                ks <- readIORef ksref
                let kb = case IntMap.lookup uid ks of
                        Nothing -> False
                        Just jk -> jk
                writeIORef ref kb
                return $ Change kb

wasd :: Signal (Double, Double)
wasd = go <~ isDown keyW ~~ isDown keyA ~~ isDown keyS ~~ isDown keyD
    where
        go w a s d = (((if d then 1 else 0) + (if a then (-1) else 0)),((if w then 1 else 0) + (if s then (-1) else 0)))

keyboard :: Signal [Key]
keyboard = Signal $ \state -> do
    let ref = keyboardRef state
    ks     <- readIORef ref
    return (cont ref, ksList ks, IntSet.fromList [0..200])
    where
        ksList = map (toEnum . fst) . filter snd . IntMap.toList
        cont ref eid
            | eid > 200 = readIORef ref >>= return . NoChange . ksList
            | otherwise = readIORef ref >>= return . Change   . ksList

keys :: Signal Key
keys = Signal $ \state -> do
    let kref = lastKeyPress state
    ref     <- newIORef keyW
    return (cont kref ref, keyW, IntSet.fromList [0..200])
    where
        cont kref ref eid
            | eid > 150 = readIORef ref >>= return . NoChange
            | otherwise = readIORef kref >>= \(k, b) -> if b
                then writeIORef ref k >>  return  (Change k)
                else readIORef  ref   >>= return . NoChange

--Collision and delay are motivating examples of events with multiple uids associated...maybe?
collision :: Signal (Map.Map UID Collision)
collision = Signal $ \_ -> return (cont, Map.empty, IntSet.empty)
    where
        cont _ = return $ NoChange $ Map.empty

--collisionMany :: (NecroFoldable t, Binary a, Eq a) => Signal (t (Entity a)) -> Signal [Maybe Collision]
--collisionMany _ = Signal $ \_ -> return (cont, [], IntSet.empty)
--    where
--        cont _ = return $ NoChange []
