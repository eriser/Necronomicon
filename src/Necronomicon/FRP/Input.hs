module Necronomicon.FRP.Input
    ( dimensions
    , mousePos
    , mouseDelta
    , mouseX
    , mouseY
    , mouseButton
    , mouseClick
    , keyToChar
    , updateTextWithChar
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
    , areDown
    , areUp
    , wasd
    , keyboard
    , collision
    ) where

import           Necronomicon.FRP.Types
import           Necronomicon.FRP.Signal
import           Necronomicon.Physics
import           Necronomicon.Graphics.Resources

import           Data.IORef
import qualified Data.IntSet                  as IntSet
import qualified Data.Map                     as Map
import qualified Graphics.UI.GLFW             as GLFW

----------------------------------
-- Input
----------------------------------

mousePos :: Signal (Double, Double)
mousePos = Signal $ \_ -> do
    ref  <- newIORef (0, 0)
    return (cont ref, (0, 0))
    where
        cont ref (MouseEvent m) = writeIORef ref m >>  return  (Change m)
        cont ref _              = readIORef  ref   >>= return . NoChange

mouseDelta :: Signal (Double, Double)
mouseDelta = Signal $ \state -> do
    GLFW.setCursorInputMode     (context $ sigResources state) GLFW.CursorInputMode'Disabled
    ref       <- newIORef ((0, 0), (0, 0))
    return (cont ref , (0, 0))
    where
        cont ref (MouseEvent (mx, my)) = do
            (px, py) <- fst <~ readIORef ref
            let delta = ((mx - px), (my - py))
            writeIORef ref ((mx, my), delta)
            return $ Change delta
        cont ref _ = readIORef ref >>= return . NoChange . snd

mouseClick :: Signal ()
mouseClick = Signal $ \_ -> do
    return (cont , ())
    where
        cont (MouseButtonEvent True)  = return $ Change   ()
        cont (MouseButtonEvent False) = return $ NoChange ()
        cont _                        = return $ NoChange ()

mouseX :: Signal Double
mouseX = fst <~ mousePos

mouseY :: Signal Double
mouseY = snd <~ mousePos

mouseButton :: Signal Bool
mouseButton = Signal $ \_ -> do
    ref  <- newIORef False
    return (cont ref, False)
    where
        cont ref (MouseButtonEvent m) = writeIORef ref m >>  return  (Change m)
        cont ref _                    = readIORef  ref   >>= return . NoChange

dimensions :: Signal (Double, Double)
dimensions = Signal $ \state -> do
    dims <- (\(x, y) -> (fromIntegral x, fromIntegral y)) <$> GLFW.getWindowSize (context $ sigResources state)
    ref  <- newIORef dims
    return (cont ref, dims)
    where
        cont ref (DimensionsEvent d) = writeIORef ref d >>  return  (Change d)
        cont ref _                   = readIORef  ref   >>= return . NoChange


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

keyToChar :: GLFW.Key -> Bool -> Char
keyToChar k isShiftDown
    | k == keyA            && not isShiftDown = 'a'
    | k == keyB            && not isShiftDown = 'b'
    | k == keyC            && not isShiftDown = 'c'
    | k == keyD            && not isShiftDown = 'd'
    | k == keyE            && not isShiftDown = 'e'
    | k == keyF            && not isShiftDown = 'f'
    | k == keyG            && not isShiftDown = 'g'
    | k == keyH            && not isShiftDown = 'h'
    | k == keyI            && not isShiftDown = 'i'
    | k == keyJ            && not isShiftDown = 'j'
    | k == keyK            && not isShiftDown = 'k'
    | k == keyL            && not isShiftDown = 'l'
    | k == keyM            && not isShiftDown = 'm'
    | k == keyN            && not isShiftDown = 'n'
    | k == keyO            && not isShiftDown = 'o'
    | k == keyP            && not isShiftDown = 'p'
    | k == keyQ            && not isShiftDown = 'q'
    | k == keyR            && not isShiftDown = 'r'
    | k == keyS            && not isShiftDown = 's'
    | k == keyT            && not isShiftDown = 't'
    | k == keyU            && not isShiftDown = 'u'
    | k == keyV            && not isShiftDown = 'v'
    | k == keyW            && not isShiftDown = 'w'
    | k == keyX            && not isShiftDown = 'x'
    | k == keyY            && not isShiftDown = 'y'
    | k == keyZ            && not isShiftDown = 'z'
    | k == key0            && not isShiftDown = '0'
    | k == key1            && not isShiftDown = '1'
    | k == key2            && not isShiftDown = '2'
    | k == key3            && not isShiftDown = '3'
    | k == key4            && not isShiftDown = '4'
    | k == key5            && not isShiftDown = '5'
    | k == key6            && not isShiftDown = '6'
    | k == key7            && not isShiftDown = '7'
    | k == key8            && not isShiftDown = '8'
    | k == key9            && not isShiftDown = '9'
    | k == keyApostrophe   && not isShiftDown = '\''
    | k == keyComma        && not isShiftDown = ','
    | k == keyMinus        && not isShiftDown = '-'
    | k == keyEqual        && not isShiftDown = '='
    | k == keyPeriod       && not isShiftDown = '.'
    | k == keySlash        && not isShiftDown = '/'
    | k == keySemiColon    && not isShiftDown = ';'
    | k == keyLeftBracket  && not isShiftDown = '['
    | k == keyBackSlash    && not isShiftDown = '\\'
    | k == keyRightBracket && not isShiftDown = ']'
    | k == keyGraveAccent  && not isShiftDown = '`'

    | k == keyA            && isShiftDown = 'A'
    | k == keyB            && isShiftDown = 'B'
    | k == keyC            && isShiftDown = 'C'
    | k == keyD            && isShiftDown = 'D'
    | k == keyE            && isShiftDown = 'E'
    | k == keyF            && isShiftDown = 'F'
    | k == keyG            && isShiftDown = 'G'
    | k == keyH            && isShiftDown = 'H'
    | k == keyI            && isShiftDown = 'I'
    | k == keyJ            && isShiftDown = 'J'
    | k == keyK            && isShiftDown = 'K'
    | k == keyL            && isShiftDown = 'L'
    | k == keyM            && isShiftDown = 'M'
    | k == keyN            && isShiftDown = 'N'
    | k == keyO            && isShiftDown = 'O'
    | k == keyP            && isShiftDown = 'P'
    | k == keyQ            && isShiftDown = 'Q'
    | k == keyR            && isShiftDown = 'R'
    | k == keyS            && isShiftDown = 'S'
    | k == keyT            && isShiftDown = 'T'
    | k == keyU            && isShiftDown = 'U'
    | k == keyV            && isShiftDown = 'V'
    | k == keyW            && isShiftDown = 'W'
    | k == keyX            && isShiftDown = 'X'
    | k == keyY            && isShiftDown = 'Y'
    | k == keyZ            && isShiftDown = 'Z'
    | k == key0            && isShiftDown = ')'
    | k == key1            && isShiftDown = '!'
    | k == key2            && isShiftDown = '@'
    | k == key3            && isShiftDown = '#'
    | k == key4            && isShiftDown = '$'
    | k == key5            && isShiftDown = '%'
    | k == key6            && isShiftDown = '^'
    | k == key7            && isShiftDown = '&'
    | k == key8            && isShiftDown = '*'
    | k == key9            && isShiftDown = '('
    | k == keyApostrophe   && isShiftDown = '\"'
    | k == keyComma        && isShiftDown = '<'
    | k == keyMinus        && isShiftDown = '_'
    | k == keyEqual        && isShiftDown = '+'
    | k == keyPeriod       && isShiftDown = '>'
    | k == keySlash        && isShiftDown = '?'
    | k == keySemiColon    && isShiftDown = ':'
    | k == keyLeftBracket  && isShiftDown = '{'
    | k == keyBackSlash    && isShiftDown = '|'
    | k == keyRightBracket && isShiftDown = '}'
    | k == keyGraveAccent  && isShiftDown = '~'

    | k == keyEnter                       = '\n'
    | k == keySpace                       = ' '
    | k == keyBackspace                   = '\b'
    | otherwise                           = '\0'

updateTextWithChar :: Char -> String -> String
updateTextWithChar '\0' ss = ss
updateTextWithChar '\b' [] = []
updateTextWithChar '\b' ss = init ss
updateTextWithChar c    ss = ss ++ [c]

keyUp :: GLFW.Key
keyUp    = GLFW.Key'Up

keyDown :: GLFW.Key
keyDown  = GLFW.Key'Down

keyLeft :: GLFW.Key
keyLeft  = GLFW.Key'Left

keyRight :: GLFW.Key
keyRight = GLFW.Key'Right

isUp :: Key -> Signal Bool
isUp k = not <~ isDown k

isDown :: Key -> Signal Bool
isDown k = Signal $ \_ -> do
    ref <- newIORef False
    return (cont ref, False)
    where
        cont ref (KeyEvent k' b) = if k == k' then writeIORef ref b >> return (Change b) else readIORef ref >>= return . NoChange
        cont ref  _              = readIORef ref >>= return . NoChange

areDown :: [Key] -> Signal Bool
areDown dks = Signal $ \_ -> do
    ref   <- newIORef False
    ksRef <- newIORef IntSet.empty
    return (cont ref ksRef, False)
    where
        cont ref ksRef (KeyEvent k b) = if null dks then return $ NoChange False else do
            ks <- readIORef ksRef
            let ks' = if b
                then IntSet.insert (fromEnum k) ks
                else IntSet.delete (fromEnum k) ks
                areDown' = foldr (\k' b' -> if not b' then b' else IntSet.member (fromEnum k') ks') True dks
            writeIORef ksRef ks'
            writeIORef ref   areDown'
            return $ Change areDown'
        cont ref _ _ = NoChange <$> readIORef ref

areUp :: [Key] -> Signal Bool
areUp dks = Signal $ \_ -> do
    ref   <- newIORef False
    ksRef <- newIORef IntSet.empty
    return (cont ref ksRef, True)
    where
        cont ref ksRef (KeyEvent k b) = do
            ks <- readIORef ksRef
            let ks' = if b
                then IntSet.insert (fromEnum k) ks
                else IntSet.delete (fromEnum k) ks
                areDown' = foldr (\k' b' -> if not b' then b' else not $ IntSet.member (fromEnum k') ks') True dks
            writeIORef ksRef ks'
            writeIORef ref   areDown'
            return $ Change areDown'
        cont ref _ _ = NoChange <$> readIORef ref

wasd :: Signal (Double, Double)
wasd = go <~ isDown keyW ~~ isDown keyA ~~ isDown keyS ~~ isDown keyD
    where
        go w a s d = (((if d then 1 else 0) + (if a then (-1) else 0)),((if w then 1 else 0) + (if s then (-1) else 0)))

keyboard :: Signal [Key]
keyboard = Signal $ \_ -> do
    ref    <- newIORef IntSet.empty
    return (cont ref, [])
    where
        cont ref (KeyEvent k b) = do
            ks <- readIORef ref
            let ks' = if b
                then IntSet.insert (fromEnum k) ks
                else IntSet.delete (fromEnum k) ks
            writeIORef ref ks'
            return $ Change $ map toEnum $ IntSet.toList ks'
        cont ref _ = readIORef ref >>= return . NoChange . map toEnum . IntSet.toList

keys :: Signal Key
keys = Signal $ \_ -> do
    ref     <- newIORef keyW
    return (cont ref, keyW)
    where
        cont ref (KeyEvent k True) = writeIORef ref k >>  return  (Change k)
        cont ref  _                = readIORef  ref   >>= return . NoChange

--Collision and delay are motivating examples of events with multiple uids associated...maybe?
collision :: Signal (Map.Map UID Collision)
collision = Signal $ \_ -> return (cont, Map.empty)
    where
        cont _ = return $ NoChange $ Map.empty
