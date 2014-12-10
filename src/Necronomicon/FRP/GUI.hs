module Necronomicon.FRP.GUI (button,
                             Gui(Gui),
                             input,
                             element,
                             gui)where

import Prelude
import Necronomicon.FRP.Signal
import Necronomicon.Linear
import Necronomicon.Graphics
import Data.IORef
import Data.Dynamic
import qualified Data.IntSet as IntSet

data Gui a = Gui a SceneObject

input :: Signal (Gui a) -> Signal a
input = lift $ \(Gui a _) -> a

element :: Signal (Gui a) -> Signal SceneObject
element = lift $ \(Gui _ s) -> s

-- data Gui a = Gui{input :: a, element :: SceneObject}

gui :: [Signal SceneObject] -> Signal ()
gui gs = render $ root <~ combine (camSig : gs)
    where
        camSig  = orthoCamera (Vector3 0 0 20) identityQuat <~ dimensions ~~ constant (RGB 0 0 0)

button :: Vector2 -> Double -> Double -> Color -> Signal (Gui Bool)
button (Vector2 x y) w h color = Signal $ \necro -> do
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
            
        st = SceneObject "" True (Vector3 x y 0) identityQuat one mt Nothing []
        sf = SceneObject "" True (Vector3 x y 0) identityQuat one mf Nothing []
        mt = Just $ Mesh [p0,p1,p2,p3,p0,p2] [color,color,color,color,color,color]
        mf = Just $ Mesh [p0,p1,p2,p3,p0,p2] [color*fc,color*fc,color*fc,color*fc,color*fc,color*fc]
        fc = RGB 0.5 0.5 0.5
        p0 = Vector3 (0 - (w * 0.5)) (0 + (h * 0.5)) 0
        p1 = Vector3 (0 - (w * 0.5)) (0 - (h * 0.5)) 0
        p2 = Vector3 (0 + (w * 0.5)) (0 - (h * 0.5)) 0
        p3 = Vector3 (0 + (w * 0.5)) (0 + (h * 0.5)) 0
        m  = Just $ Mesh [p0,p1,p2,p3,p0,p2] [color,color,color,color,color,color]
        hw = w * 0.5 * 1
        hh = h * 0.5 * 1


