module Necronomicon.FRP.GUI (button,
                             Gui(Gui,input,element))where

import Prelude
import Necronomicon.FRP.Signal
import Necronomicon.Linear
import Necronomicon.Graphics

data Gui a = Gui{input :: a, element :: SceneObject}

button :: Vector2 -> Double -> Double -> Color -> Signal (Gui ())
button (Vector2 x y) w h color = pure $ Gui () $ SceneObject "" True (Vector3 x y 0) identityQuat one m Nothing []
    where
        p0 = Vector3 (0 - (w * 0.5)) (0 + (h * 0.5)) 0
        p1 = Vector3 (0 - (w * 0.5)) (0 - (h * 0.5)) 0
        p2 = Vector3 (0 + (w * 0.5)) (0 - (h * 0.5)) 0
        p3 = Vector3 (0 + (w * 0.5)) (0 + (h * 0.5)) 0
        m  = Just $ Mesh [p0,p1,p2,p3,p0,p2] [color,color,color,color,color,color]


