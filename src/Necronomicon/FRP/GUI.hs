module Necronomicon.FRP.GUI (button,
                             Gui(Gui,input,element))where

import Prelude
import Necronomicon.FRP.Signal
import Necronomicon.Linear
import Necronomicon.Graphics
import Data.IORef
import Data.Dynamic
import qualified Data.IntSet as IntSet

data Gui a = Gui{input :: a, element :: SceneObject}

button :: Vector2 -> Double -> Double -> Color -> Signal (Bool,SceneObject)
button (Vector2 x y) w h color = Signal $ \necro -> do
    ref <- newIORef False
    mpr <- newIORef (0,0)
    return ((False,sf),processEvent ref mpr,IntSet.fromList [0,1])
    where
        processEvent ref mpr (Event uid val)
            | uid == 0 = case fromDynamic val of
                Nothing -> print "button Type error" >> (return $ NoChange (False,sf))
                Just m  -> do
                    writeIORef mpr m
                    v <- readIORef ref
                    case v of
                        True  -> return $ NoChange (v,st)
                        False -> return $ NoChange (v,sf)
            | uid == 1 = case fromDynamic val of
                Nothing -> print "button Type error" >> (return $ NoChange (False,sf))
                Just v  -> case v of
                    False -> writeIORef ref False >> (return $ Change (False,sf))
                    True  -> do
                        (mx,my) <- readIORef mpr
                        case mx >= x - hw && mx <= x + hw && my >= y - hh && my <= y + hh of
                           True  -> writeIORef ref True >> (return $ Change (True,st))
                           False -> return $ NoChange (False,sf)
            | otherwise = do
                v <- readIORef ref
                case v of
                    True  -> return $ NoChange (v,st)
                    False -> return $ NoChange (v,sf)
            
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
        hw = w * 0.5 * 0.1
        hh = h * 0.5 * 0.1


