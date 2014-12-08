module Necronomicon.FRP.GUI (button)where

import Prelude
import Necronomicon.FRP.Signal
import Necronomicon.Linear
import Necronomicon.Graphics

data Gui a = Gui{input :: a, element :: SceneObject}

button :: Vector3 -> (Double,Double) -> Color -> Signal (Gui ())
button pos (mixy,maxy) color = pure $ Gui () $ SceneObject "" True pos identityQuat one m Nothing []
    where
        m = Just $ Mesh
             [Vector3 (-0.4) (-0.3) 0,
              Vector3   0.4  (-0.3) 0,
              Vector3     0    0.3  0,

              Vector3 (-0.4) (-0.3) 0,
              Vector3   0.4  (-0.3) 0,
              Vector3     0    0.3  0]
             [color,
              color,
              color]

-- rect pos r chldn = SceneObject "" True pos r one m Nothing []
    -- where
        -- m = Just $ Mesh
             -- [Vector3 (-0.4) (-0.3) 0,
              -- Vector3   0.4  (-0.3) 0,
              -- Vector3     0    0.3  0]
             -- [RGB 1 0 0,
              -- RGB 0 1 0,
              -- RGB 0 0 1]



