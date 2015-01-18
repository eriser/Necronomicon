
module Necronomicon.FRP.GUI (Gui(..),
                             Size(..),
                             button,
                             input,
                             element,
                             gui,
                             label,
                             slider,
                             textEdit)where

import           Data.Dynamic
import qualified Data.IntSet             as IntSet
import           Data.IORef
import           Necronomicon.FRP.Signal
import           Necronomicon.Graphics
import           Necronomicon.Linear
import           Necronomicon.Util       (loadTextureFromTGA)
import           Prelude

data Gui a = Gui a SceneObject
data Size = Size Double Double

input :: Signal (Gui a) -> Signal a
input = lift $ \(Gui a _) -> a

element :: Signal (Gui a) -> Signal SceneObject
element = lift $ \(Gui _ s) -> s

gui :: [Signal SceneObject] -> Signal ()
gui gs = render $ root <~ combine (pure cam : gs)
    where
        cam = orthoCamera 0 identity black

label :: Vector2 -> Font -> Color -> String -> SceneObject
label (Vector2 x y) font color text = SceneObject (Vector3 x y 0) identity 1 (drawText text font ambient) []

textEdit :: Vector2 -> Size -> Font -> Color -> Signal SceneObject
textEdit (Vector2 x y) (Size w h) font color = pure background
    where
        background = SceneObject (Vector3  x       y    0) identity 1 (Model (rect w h) (vertexColored color)) [textObject]
        textObject = SceneObject (Vector3 (-w/2.05)  (h/2) 1) identity 1 (drawBoundText text font ambient (w * 0.9,h)) []
        text       = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Mauris ipsum risus, luctus vel mollis non, hendrerit non mi. Sed at blandit ex. Donec aliquam pellentesque convallis. Integer in nisl ut ipsum dignissim vestibulum. Aenean porta nunc magna, id porttitor quam scelerisque non. Ut malesuada mi lectus, vitae finibus est lacinia nec. Nunc varius sodales porttitor. Nam faucibus tortor quis ullamcorper feugiat. Etiam mollis tellus mi, pretium consequat justo suscipit in. Etiam posuere placerat risus, eget efficitur nulla. Integer non leo vitae justo egestas consequat."

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

        s  h = SceneObject (Vector3 x y 0)    identity 1 sm [v h]
        v  h = SceneObject (Vector3 0 0 0.01) identity 1 (vm h) []
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

        st = SceneObject (Vector3 x y 0) identity 1 (m color) []
        sf = SceneObject (Vector3 x y 0) identity 1 (m fc   ) []
        fc = RGB 0.5 0.5 0.5
        hw = w * 0.5
        hh = h * 0.5
        m c= Model (rect w h) (vertexColored c)
        --c is color....
