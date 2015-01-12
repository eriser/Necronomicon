
module Necronomicon.FRP.GUI (button,
                             Gui(Gui),
                             input,
                             element,
                             gui,
                             label,
                             slider,
                             Size(Size))where

import Prelude
import Necronomicon.FRP.Signal
import Necronomicon.Linear
import Necronomicon.Graphics
import Necronomicon.Util (loadTextureFromTGA)
import Data.IORef
import Data.Dynamic
import qualified Data.IntSet as IntSet

data Gui a = Gui a SceneObject
data Size = Size Double Double

input :: Signal (Gui a) -> Signal a
input = lift $ \(Gui a _) -> a

element :: Signal (Gui a) -> Signal SceneObject
element = lift $ \(Gui _ s) -> s

gui :: [Signal SceneObject] -> Signal ()
gui gs = render $ root <~ combine (camSig : gs)
    where
        camSig = orthoCamera (Vector3 0 0 0) identityQuat <~ dimensions ~~ constant (RGB 0 0 0)

label :: Vector2 -> Font -> Color ->  String -> SceneObject
label (Vector2 x y) font@(Font _ size) color text = SceneObject "" True (Vector3 x y 0) identityQuat (fromIntegral size / 400) (Just $ drawText text font ambient) Nothing []

guiEvent :: (Typeable a) => IORef (Gui b) -> Dynamic -> (a -> IO (EventValue (Gui b))) -> IO (EventValue (Gui b))
guiEvent ref v f = case fromDynamic v of
    Nothing -> print "button Type error" >> readIORef ref >>= return . NoChange
    Just v' -> f v'

slider :: Vector2 -> Size -> Color -> Signal (Gui Double)
slider (Vector2 x y) (Size w h) color = Signal $ \necro -> do
    ref <- newIORef $ Gui 0.0 (s 0.5)
    mpr <- newIORef False
    return (Gui 0.0 (s 0.5),processEvent ref mpr,IntSet.fromList [0,1])
    where
        processEvent ref mpr (Event uid val)
            | uid == 1 = guiEvent ref val $ \mp -> writeIORef mpr mp >> readIORef ref >>= return . NoChange
            | uid == 0 = guiEvent ref val $ \(x,y)  -> do
                isMouseDown <- readIORef mpr
                case isMouseDown     &&
                     x >= 0.5 - hw   &&
                     x <= 0.5 + hw   &&
                     y >= 0.5 - hh h &&
                     y <= 0.5 + hh h of
                    False -> readIORef ref >>= return . NoChange
                    True  -> do
                        let v = linlin (0.5 - hh h) (0.5 + hh h) 1 0 y
                        let g = Gui v (s v)
                        writeIORef ref g
                        return $ Change g
            | otherwise = readIORef ref >>= return . NoChange

        s  h = SceneObject "" True (Vector3 x y 0)    identityQuat 1 sm Nothing [v h]
        v  h = SceneObject "" True (Vector3 0 0 0.01) identityQuat 1 (vm h) Nothing []
        p0 v = Vector3 (0 - hw) (hh h - v * h) 0
        p1 v = Vector3 (0 - hw) (0 + hh h) 0
        p2 v = Vector3 (0 + hw) (0 + hh h) 0
        p3 v = Vector3 (0 + hw) (hh h - v * h) 0
        hw   = w * 0.5
        hh h = h * 0.5
        fc   = RGB 0.5 0.5 0.5
        -- sm   = SimpleMesh [p0 1,p1 1,p2 1,p3 1,p0 1,p2 1] [color,color,color,color,color,color]
        -- vm h = SimpleMesh [p0 h,p1 h,p2 h,p3 h,p0 h,p2 h] [color*fc,color*fc,color*fc,color*fc,color*fc,color*fc]
        sm = undefined
        vm h = undefined

button :: Vector2 -> Size -> Color -> Signal (Gui Bool)
button (Vector2 x y) (Size w h) color = Signal $ \necro -> do
    ref <- newIORef False
    mpr <- newIORef (0,0)
    return (Gui False sf,processEvent ref mpr,IntSet.fromList [0,1])
    where
        processEvent ref mpr (Event uid val)
            | uid == 0 = case fromDynamic val of
                Nothing -> print "button Type error" >> (return $ NoChange (Gui False sf))
                Just m  -> do
                    writeIORef mpr m
                    v <- readIORef ref
                    case v of
                        True  -> return $ NoChange (Gui v st)
                        False -> return $ NoChange (Gui v sf)
            | uid == 1 = case fromDynamic val of
                Nothing -> print "button Type error" >> (return $ NoChange (Gui False sf))
                Just v  -> case v of
                    False -> writeIORef ref False >> (return $ Change (Gui False sf))
                    True  -> do
                        (mx,my) <- readIORef mpr
                        case mx >= x - hw && mx <= x + hw && my >= y - hh && my <= y + hh of
                           True  -> writeIORef ref True >> (return $ Change (Gui True st))
                           False -> return $ NoChange (Gui False sf)
            | otherwise = do
                v <- readIORef ref
                case v of
                    True  -> return $ NoChange (Gui v st)
                    False -> return $ NoChange (Gui v sf)
            
        st = SceneObject "" True (Vector3 x y 0) identityQuat 1 (Just $ m color) Nothing []
        sf = SceneObject "" True (Vector3 x y 0) identityQuat 1 (Just $ m fc   ) Nothing []
        fc = RGB 0.5 0.5 0.5
        hw = w * 0.5
        hh = h * 0.5
        m c= Model (rect w h) vertexColored
        --c is color....

