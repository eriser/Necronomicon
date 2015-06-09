{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Necronomicon.Game where

-- import Debug.Trace
import Test.QuickCheck
import Data.List (sort)
import Control.Concurrent
import Graphics.Rendering.OpenGL.Raw
import qualified Data.Vector as V
import qualified Graphics.Rendering.OpenGL         as GL
import qualified Graphics.UI.GLFW                  as GLFW

import Necronomicon.Linear
import Necronomicon.Physics
import Necronomicon.Physics.DynamicTree
import Necronomicon.Graphics
import Necronomicon.Utility              (getCurrentTime)

import Data.Binary
import GHC.Generics

-------------------------------------------------------
-- TODO: Ideas
-- Clockwork world / minature-golem-esque world?
-------------------------------------------------------



-------------------------------------------------------
-- GameObject
-------------------------------------------------------

-- data Transform  = Transform Vector3 Quaternion Vector3 deriving (Show)
data GameObject = GameObject {
    gid      :: UID,
    pos      :: Vector3,
    rot      :: Quaternion,
    gscale   :: Vector3,
    collider :: Maybe Collider,
    model    :: Maybe Model,
    camera   :: Maybe Camera,
    children :: [GameObject]
} deriving (Show, Eq)

instance Binary GameObject where
    put (GameObject uid p r s c m cam cs) = put uid >> put p >> put r >> put s >> put c >> put m >> put cam >> put cs
    get                                   = GameObject <$> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get

-------------------------------------------------------
-- GameObject API
-------------------------------------------------------

mkGameObject :: GameObject
mkGameObject = GameObject New 0 identity 1 Nothing Nothing Nothing []

rotate :: Vector3 -> GameObject -> GameObject
rotate (Vector3 x y z) g = g{rot = rot g * fromEuler x y z}

move :: Vector3 -> GameObject -> GameObject
move dir g = g{pos = pos g + dir}

translate :: Vector3 -> GameObject -> GameObject
-- translate dir g = g{pos = pos g + transformVector (rot g) dir}
translate dir g = g{pos = pos g + (dir .*. rotFromQuaternion (rot g))}

collisions :: GameObject -> [Collision]
collisions g
    | Just c <- collider g = colliderCollisions c
    | otherwise            = []

gchildren_ :: [GameObject] -> GameObject -> GameObject
gchildren_ cs (GameObject uid p r s c m cm _) = GameObject uid p r s c m cm cs

-------------------------------------------------------
-- GameObject - Getters / Setters
-------------------------------------------------------

rotMat :: GameObject -> Matrix3x3
rotMat (GameObject _ _ r _ _ _ _ _) = rotFromQuaternion r

transMat :: GameObject -> Matrix4x4
transMat (GameObject _ p r s _ _ _ _) = trsMatrix p r s

collider_ :: Collider -> GameObject -> GameObject
collider_ c (GameObject u p r s _ m cm cs) = GameObject u p r s (Just c) m cm cs

gaddChild :: GameObject -> GameObject -> GameObject
gaddChild g (GameObject u p r s c m cm cs) = GameObject u p r s c m cm (g : cs)

removeChild :: GameObject -> Int -> GameObject
removeChild (GameObject u p r s c m cm cs) n
    | null cs2  = GameObject u p r s c m cm cs
    | otherwise = GameObject u p r s c m cm $ cs1 ++ tail cs2
    where
        (cs1, cs2) = splitAt n cs

-------------------------------------------------------
-- GameObject - Folding / Mapping
-------------------------------------------------------

foldChildren :: (GameObject -> a -> a) -> a -> GameObject -> a
foldChildren f acc g = foldr (\c acc' -> foldChildren f acc' c) (f g acc) (children g)

mapFold :: ((GameObject, a) -> (GameObject, a)) -> (GameObject, a) -> (GameObject, a)
mapFold f gacc = (  gchildren_ gcs g, acc')
    where
        (g,   acc)      = f gacc
        (gcs, acc')     = foldr mapC ([], acc) (children g)
        mapC c (cs, cacc) = (c' : cs, cacc')
            where
                (c', cacc') = mapFold f (c, cacc)

mapFoldStack :: ((GameObject, a, s) -> (GameObject, a, s)) -> (GameObject, a, s) -> (GameObject, a)
mapFoldStack f gacc = (gchildren_ gcs g, acc')
    where
        (g,   acc, s)     = f gacc
        (gcs, acc')       = foldr mapC ([], acc) (children g)
        mapC c (cs, cacc) = (c' : cs, cacc')
            where
                (c', cacc') = mapFoldStack f (c, cacc, s)

-------------------------------------------------------
-- Update
-------------------------------------------------------

{-
    update:
        The main update loop.
        Transforms the game object tree supplied by the client program-
        into a new gameObject tree containing collision events and physics simulations.
-}
update :: (GameObject, DynamicTree) -> (GameObject, DynamicTree)
update (g, tree) = (g', bulkUpdate ui)
    where
        (g', ui) = mapFoldStack genUpdateInput (g, (([],0), [], tree), identity4)

{-
    genUpdateInput:
        Transform the input gameObject into update information for a dynamicTree transformation
        We've got to get the newly consumed ids to the correct gameobjects....
-}
-- the issue is with the matrix multiplication!
-- Need to figure out how to clip an affine vector4 back to 3D space
genUpdateInput :: (GameObject, UpdateInput, Matrix4x4) -> (GameObject, UpdateInput, Matrix4x4)
genUpdateInput (g, (d, il, t), world)
    | Just col <- collider g = updateFromCollider col
    | otherwise              = (g, (d, il, t), newWorld)
    where
        newWorld             = world .*. transMat g
        updateFromCollider col
            | UID uid <- cid = (g'' , (nodeListCons (caabb, uid) d,               il, t), newWorld)
            | otherwise      = (g',   (nodeListCons (caabb,   i) d, (caabb', i) : il, t{freeList = fl}), newWorld)
            where
                g'       = collider_ col' g
                g''      = collider_ (colliderTransform_ newWorld $ col) g
                cid      = colliderID  col
                col'     = colliderTransform_ newWorld $ colliderID_ (UID i) col
                caabb    = colliderAABB col
                caabb'   = enlargeAABB  caabb
                (i : fl) = freeList t

-------------------------------------------------------
-- Rendering
-------------------------------------------------------

drawGame :: Matrix4x4 -> Matrix4x4 -> Matrix4x4 -> Resources -> Bool -> GameObject -> IO ()
drawGame world view proj resources debug g = do
    newWorld <- drawG world view proj resources debug  g
    mapM_ (drawGame newWorld view proj resources debug) (children g)

drawG :: Matrix4x4 -> Matrix4x4 -> Matrix4x4 -> Resources -> Bool -> GameObject -> IO Matrix4x4
drawG world view proj resources debug g
    | Just (Model mesh mat) <- model g    = drawMeshWithMaterial mat mesh modelView proj resources >> return newWorld
    | Just (FontRenderer text font mat) <- model g = do
        (fontTexture, fontMesh) <- renderFont text font resources
        drawMeshWithMaterial (setEmptyTextures fontTexture mat) fontMesh modelView proj resources >> return newWorld
    | debug, Just c  <- collider g = debugDrawCollider c           view      proj resources >> return newWorld
    | otherwise                    = return newWorld
    where
        newWorld  = world .*. transMat g
        modelView = view  .*. newWorld

--From Camera.hs
renderCameraG :: (Int,Int) -> Matrix4x4 -> GameObject -> Resources -> Bool -> GameObject -> DynamicTree -> IO Matrix4x4
renderCameraG (w,h) view scene resources debug so t = case camera so of
    Nothing -> return newView
    Just c  -> do
        let  ratio         = fromIntegral w / fromIntegral h
        let (RGBA r g b a) = case _clearColor c of
                RGB r' g' b' -> RGBA r' g' b' 1.0
                c'           -> c'

        --If we have anye post-rendering fx let's bind their fbo
        case _fx c of
            []   -> return ()
            fx:_ -> getPostFX resources (fromIntegral w,fromIntegral h) fx >>= \postFX -> glBindFramebuffer gl_FRAMEBUFFER (postRenderFBO postFX)

        GL.depthFunc     GL.$= Just GL.Less
        GL.blend         GL.$= GL.Enabled
        GL.blendBuffer 0 GL.$= GL.Enabled
        GL.blendFunc     GL.$= (GL.SrcAlpha,GL.OneMinusSrcAlpha)

        GL.clearColor GL.$= GL.Color4 (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a)
        GL.clear [GL.ColorBuffer,GL.DepthBuffer]

        GL.viewport GL.$= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
        GL.loadIdentity

        case _fov c of
            0 -> drawGame identity4 (invert newView) (orthoMatrix 0 ratio 1 0 (-1) 1) resources debug scene
            _ -> do
                let invView = invert newView
                drawGame identity4 invView (perspMatrix (_fov c) ratio (_near c) (_far c)) resources debug scene
                if debug then debugDrawDynamicTree t invView (perspMatrix (_fov c) ratio (_near c) (_far c)) resources else return ()

        mapM_ (drawPostRenderFX (RGBA r g b a)) $ _fx c

        return $ newView
    where
        newView = view .*. transMat so
        drawPostRenderFX (RGB r g b) fx = drawPostRenderFX (RGBA r g b 1) fx
        drawPostRenderFX (RGBA r g b a) fx = do
            glBindFramebuffer gl_FRAMEBUFFER 0
            GL.depthFunc     GL.$= Nothing
            GL.blend         GL.$= GL.Disabled
            GL.blendBuffer 0 GL.$= GL.Disabled

            GL.clearColor GL.$= GL.Color4 (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a)
            GL.clear [GL.ColorBuffer,GL.DepthBuffer]
            postFX <- getPostFX resources (fromIntegral w,fromIntegral h) fx
            let postFXMat = setEmptyTextures (LoadedTexture $ GL.TextureObject $ postRenderTex postFX) (postRenderMaterial postFX)
            drawMeshWithMaterial postFXMat (rect 1 1) identity4 (orthoMatrix 0 1 0 1 (-1) 1) resources

            GL.depthFunc     GL.$= Just GL.Less
            GL.blend         GL.$= GL.Enabled
            GL.blendBuffer 0 GL.$= GL.Enabled
            GL.blendFunc     GL.$= (GL.SrcAlpha,GL.OneMinusSrcAlpha)

renderCamerasG :: (Int,Int) -> Matrix4x4 -> GameObject -> Resources -> Bool -> DynamicTree -> GameObject -> IO ()
renderCamerasG (w,h) view scene resources debug t g = renderCameraG (w,h) view scene resources debug g t >>= \newView -> mapM_ (renderCamerasG (w,h) newView scene resources debug t) (children g)

renderGraphicsG :: GLFW.Window -> Resources -> Bool -> GameObject -> GameObject -> DynamicTree -> IO ()
renderGraphicsG window resources debug scene _ t = do
    (w,h) <- GLFW.getWindowSize window

    --render scene
    renderCamerasG (w,h) identity4 scene resources debug t scene

    --render gui
    -- drawGame identity4 identity4 (orthoMatrix 0 (fromIntegral w / fromIntegral h) 1 0 (-1) 1) resources False gui

    GLFW.swapBuffers window
    GLFW.pollEvents
    -- GL.flush

runGame :: (Scene a, Binary a, Show a) => (World -> a -> a) -> a -> IO()
runGame f inits = initWindow (1920, 1080) True >>= \mw -> case mw of
    Nothing -> print "Error starting GLFW." >> return ()
    Just w  -> do
        putStrLn "Starting Necronomicon"

        GLFW.setCursorInputMode w GLFW.CursorInputMode'Hidden

        resources   <- newResources
        currentTime <- getCurrentTime
        let world = mkWorld{runTime = currentTime}
        renderNecronomicon False w resources inits empty world
    where
        renderNecronomicon quit window resources s tree world
            | quit      = print "Qutting" >> return ()
            | otherwise = do

                GLFW.pollEvents
                qKey <- (== GLFW.KeyState'Pressed) <$> GLFW.getKey window GLFW.Key'Escape
                wKey <- (== GLFW.KeyState'Pressed) <$> GLFW.getKey window GLFW.Key'W
                aKey <- (== GLFW.KeyState'Pressed) <$> GLFW.getKey window GLFW.Key'A
                sKey <- (== GLFW.KeyState'Pressed) <$> GLFW.getKey window GLFW.Key'S
                dKey <- (== GLFW.KeyState'Pressed) <$> GLFW.getKey window GLFW.Key'D
                mb   <- (== GLFW.MouseButtonState'Pressed) <$> GLFW.getMouseButton window GLFW.MouseButton'1
                (ww, wh) <- (\(wd, hd) -> (fromIntegral wd, fromIntegral hd)) <$> GLFW.getWindowSize window
                mp       <- (\(cx, cy) -> ((cx - ww * 0.5) / ww, (cy - wh * 0.5) / wh)) <$> GLFW.getCursorPos window
                currentTime <- getCurrentTime

                GLFW.setCursorInputMode window GLFW.CursorInputMode'Hidden

                let world' = world {
                        deltaTime       = currentTime - runTime world,
                        runTime         = currentTime,

                        --Signal Style
                        moveKeys        = (if aKey then -1 else 0 + if dKey then 1 else 0, if wKey then 1 else 0 + if sKey then -1 else 0),
                        mouseIsDown     = mb,
                        mousePosition   = mp,

                        --Event Style
                        moveKeysPressed = if moveKeys world /= moveKeys world' then Just (if aKey then -1 else 0 + if dKey then 1 else 0, if wKey then 1 else 0 + if sKey then -1 else 0) else Nothing,
                        mouseClicked    = mb && not (mouseIsDown world),
                        mouseMoved      = if mp /= mousePosition world then Just mp else Nothing
                    }
                    s'          = f world' s
                    g           = gchildren_ (getGameObjects s' []) mkGameObject
                    (g', tree') = update (g, tree)
                    (s'', _)    = setGameObjects s' (children g')

                GLFW.setCursorPos window (ww * 0.5) (wh * 0.5)
                GLFW.setCursorInputMode window GLFW.CursorInputMode'Hidden

                renderGraphicsG window resources True g' g' tree'
                threadDelay $ 16667
                renderNecronomicon qKey window resources s'' tree' world'

-------------------------------------------------------
-- Entity
-------------------------------------------------------

data World = World {
    --SignalStyle
    runTime       :: Double,
    deltaTime     :: Double,
    mousePosition :: (Double, Double),
    moveKeys      :: (Double, Double),
    mouseIsDown   :: Bool,

    --Event Style
    mouseClicked    :: Bool,
    mouseMoved      :: Maybe (Double, Double),
    moveKeysPressed :: Maybe (Double, Double)
}

mkWorld :: World
mkWorld = World 0 0 (0, 0) (0, 0) False False Nothing Nothing

data Entity a = Entity {
    userData   :: a,
    gameObject :: GameObject
} deriving (Show, Eq)

instance Binary a => Binary (Entity a) where
    put (Entity a g) = put a >> put g
    get              = Entity <$> get <*> get

class Scene a where
    getGameObjects :: a -> [GameObject] -> [GameObject]
    setGameObjects :: a -> [GameObject] -> (a, [GameObject])

    default getGameObjects :: (Generic a, GScene (Rep a)) => a -> [GameObject] -> [GameObject]
    getGameObjects x gs = getGameObjectsG (from x) gs

    default setGameObjects :: (Generic a, GScene (Rep a)) => a -> [GameObject] -> (a, [GameObject])
    setGameObjects x gs = (to x', gs') where (x', gs') = setGameObjectsG (from x) gs

instance Scene (Entity a) where
    getGameObjects (Entity _ g) gs = g : gs
    setGameObjects (Entity a g) [] = (Entity a g, [])
    setGameObjects (Entity a _) gs = (Entity a $ head gs, tail gs)

instance Scene a => Scene [a] where
    getGameObjects es gs = foldr (\e gs' -> getGameObjects e gs') gs es
    setGameObjects es gs = fmap reverse $ foldl foldE ([], gs) es
        where
            foldE (es', gs') e = (e' : es', gs'')
                where
                    (e', gs'') = setGameObjects e gs'

instance (Scene a, Scene b) => Scene (a, b) where
    getGameObjects (e1, e2) gs  = getGameObjects e1 $ getGameObjects e2 gs
    setGameObjects (e1, e2) gs1 = ((e1', e2'), gs3)
        where
            (e1', gs2) = setGameObjects e1 gs1
            (e2', gs3) = setGameObjects e2 gs2

instance (Scene a, Scene b, Scene c) => Scene (a, b, c) where
    getGameObjects (e1, e2, e3) gs  = getGameObjects e1 $ getGameObjects e2 $ getGameObjects e3 gs
    setGameObjects (e1, e2, e3) gs1 = ((e1', e2', e3'), gs4)
        where
            (e1', gs2) = setGameObjects e1 gs1
            (e2', gs3) = setGameObjects e2 gs2
            (e3', gs4) = setGameObjects e3 gs3

instance (Scene a, Scene b, Scene c, Scene d) => Scene (a, b, c, d) where
    getGameObjects (e1, e2, e3, e4) gs  = getGameObjects e1 $ getGameObjects e2 $ getGameObjects e3 $ getGameObjects e4 gs
    setGameObjects (e1, e2, e3, e4) gs1 = ((e1', e2', e3', e4'), gs5)
        where
            (e1', gs2) = setGameObjects e1 gs1
            (e2', gs3) = setGameObjects e2 gs2
            (e3', gs4) = setGameObjects e3 gs3
            (e4', gs5) = setGameObjects e4 gs4

instance (Scene a, Scene b, Scene c, Scene d, Scene e) => Scene (a, b, c, d, e) where
    getGameObjects (e1, e2, e3, e4, e5) gs  = getGameObjects e1 $ getGameObjects e2 $ getGameObjects e3 $ getGameObjects e4 $ getGameObjects e5 gs
    setGameObjects (e1, e2, e3, e4, e5) gs1 = ((e1', e2', e3', e4', e5'), gs6)
        where
            (e1', gs2) = setGameObjects e1 gs1
            (e2', gs3) = setGameObjects e2 gs2
            (e3', gs4) = setGameObjects e3 gs3
            (e4', gs5) = setGameObjects e4 gs4
            (e5', gs6) = setGameObjects e5 gs5

instance (Scene a, Scene b, Scene c, Scene d, Scene e, Scene f) => Scene (a, b, c, d, e, f) where
    getGameObjects (e1, e2, e3, e4, e5, e6) gs  = getGameObjects e1 $ getGameObjects e2 $ getGameObjects e3 $ getGameObjects e4 $ getGameObjects e5 $ getGameObjects e6 gs
    setGameObjects (e1, e2, e3, e4, e5, e6) gs1 = ((e1', e2', e3', e4', e5', e6'), gs7)
        where
            (e1', gs2) = setGameObjects e1 gs1
            (e2', gs3) = setGameObjects e2 gs2
            (e3', gs4) = setGameObjects e3 gs3
            (e4', gs5) = setGameObjects e4 gs4
            (e5', gs6) = setGameObjects e5 gs5
            (e6', gs7) = setGameObjects e6 gs6

instance (Scene a, Scene b, Scene c, Scene d, Scene e, Scene f, Scene g) => Scene (a, b, c, d, e, f, g) where
    getGameObjects (e1, e2, e3, e4, e5, e6, e7) gs  = getGameObjects e1 $ getGameObjects e2 $ getGameObjects e3 $ getGameObjects e4 $ getGameObjects e5 $ getGameObjects e6 $ getGameObjects e7 gs
    setGameObjects (e1, e2, e3, e4, e5, e6, e7) gs1 = ((e1', e2', e3', e4', e5', e6', e7'), gs8)
        where
            (e1', gs2) = setGameObjects e1 gs1
            (e2', gs3) = setGameObjects e2 gs2
            (e3', gs4) = setGameObjects e3 gs3
            (e4', gs5) = setGameObjects e4 gs4
            (e5', gs6) = setGameObjects e5 gs5
            (e6', gs7) = setGameObjects e6 gs6
            (e7', gs8) = setGameObjects e7 gs7

instance (Scene a, Scene b, Scene c, Scene d, Scene e, Scene f, Scene g, Scene h) => Scene (a, b, c, d, e, f, g, h) where
    getGameObjects (e1, e2, e3, e4, e5, e6, e7, e8) gs  =
        getGameObjects e1 $
        getGameObjects e2 $
        getGameObjects e3 $
        getGameObjects e4 $
        getGameObjects e5 $
        getGameObjects e6 $
        getGameObjects e7 $
        getGameObjects e8 gs
    setGameObjects (e1, e2, e3, e4, e5, e6, e7, e8) gs1 = ((e1', e2', e3', e4', e5', e6', e7', e8'), gs9)
        where
            (e1', gs2) = setGameObjects e1 gs1
            (e2', gs3) = setGameObjects e2 gs2
            (e3', gs4) = setGameObjects e3 gs3
            (e4', gs5) = setGameObjects e4 gs4
            (e5', gs6) = setGameObjects e5 gs5
            (e6', gs7) = setGameObjects e6 gs6
            (e7', gs8) = setGameObjects e7 gs7
            (e8', gs9) = setGameObjects e8 gs8

instance (Scene a, Scene b, Scene c, Scene d, Scene e, Scene f, Scene g, Scene h, Scene i) => Scene (a, b, c, d, e, f, g, h, i) where
    getGameObjects (e1, e2, e3, e4, e5, e6, e7, e8, e9) gs  =
        getGameObjects e1 $
        getGameObjects e2 $
        getGameObjects e3 $
        getGameObjects e4 $
        getGameObjects e5 $
        getGameObjects e6 $
        getGameObjects e7 $
        getGameObjects e8 $
        getGameObjects e9 gs
    setGameObjects (e1, e2, e3, e4, e5, e6, e7, e8, e9) gs1 = ((e1', e2', e3', e4', e5', e6', e7', e8', e9'), gs10)
        where
            (e1', gs2)  = setGameObjects e1 gs1
            (e2', gs3)  = setGameObjects e2 gs2
            (e3', gs4)  = setGameObjects e3 gs3
            (e4', gs5)  = setGameObjects e4 gs4
            (e5', gs6)  = setGameObjects e5 gs5
            (e6', gs7)  = setGameObjects e6 gs6
            (e7', gs8)  = setGameObjects e7 gs7
            (e8', gs9)  = setGameObjects e8 gs8
            (e9', gs10) = setGameObjects e9 gs9

instance (Scene a, Scene b, Scene c, Scene d, Scene e, Scene f, Scene g, Scene h, Scene i, Scene j) => Scene (a, b, c, d, e, f, g, h, i, j) where
    getGameObjects (e1, e2, e3, e4, e5, e6, e7, e8, e9, e10) gs  =
        getGameObjects e1 $
        getGameObjects e2 $
        getGameObjects e3 $
        getGameObjects e4 $
        getGameObjects e5 $
        getGameObjects e6 $
        getGameObjects e7 $
        getGameObjects e8 $
        getGameObjects e9 $
        getGameObjects e10 gs
    setGameObjects (e1, e2, e3, e4, e5, e6, e7, e8, e9, e10) gs1 = ((e1', e2', e3', e4', e5', e6', e7', e8', e9', e10'), gs11)
        where
            (e1',  gs2)  = setGameObjects e1  gs1
            (e2',  gs3)  = setGameObjects e2  gs2
            (e3',  gs4)  = setGameObjects e3  gs3
            (e4',  gs5)  = setGameObjects e4  gs4
            (e5',  gs6)  = setGameObjects e5  gs5
            (e6',  gs7)  = setGameObjects e6  gs6
            (e7',  gs8)  = setGameObjects e7  gs7
            (e8',  gs9)  = setGameObjects e8  gs8
            (e9',  gs10) = setGameObjects e9  gs9
            (e10', gs11) = setGameObjects e10 gs10

class GScene f where
    getGameObjectsG :: f a -> [GameObject] -> [GameObject]
    setGameObjectsG :: f a -> [GameObject] -> (f a, [GameObject])

--Generic Scene Instances
instance GScene V1 where
    getGameObjectsG _ gs = gs
    setGameObjectsG _ gs = (undefined, gs)

instance GScene U1 where
    getGameObjectsG _ gs = gs
    setGameObjectsG _ gs = (U1, gs)

instance (GScene f, GScene g) => GScene ((:+:) f g) where
    getGameObjectsG (L1 x) gs = getGameObjectsG x gs
    getGameObjectsG (R1 x) gs = getGameObjectsG x gs
    setGameObjectsG (L1 x) gs = (L1 x', gs') where (x', gs') = setGameObjectsG x gs
    setGameObjectsG (R1 x) gs = (R1 x', gs') where (x', gs') = setGameObjectsG x gs

instance (GScene f, GScene g) => GScene ((:*:) f g) where
    getGameObjectsG (x :*: y) gs = getGameObjectsG x $ getGameObjectsG y gs
    setGameObjectsG (x :*: y) gs = (x' :*: y', gs3)
        where
            (x', gs2) = setGameObjectsG x gs
            (y', gs3) = setGameObjectsG y gs2

instance (Scene c) => GScene (K1 i c) where
    getGameObjectsG (K1 x) gs = getGameObjects x gs
    setGameObjectsG (K1 x) gs = (K1 x', gs') where (x', gs') = setGameObjects x gs

instance (GScene f) => GScene (M1 i t f) where
    getGameObjectsG (M1 x) gs = getGameObjectsG x gs
    setGameObjectsG (M1 x) gs = (M1 x', gs') where (x', gs') = setGameObjectsG x gs

-- state timing convenience functions
data Timer = Timer { timerStartTime :: Double, timerEndTime :: Double } deriving (Show)

instance Binary Timer where
    put (Timer s e) = put s >> put e
    get             = Timer <$> get <*> get

timer :: Double -> World -> Timer
timer t w = Timer (runTime w) (runTime w + t)

timerReady :: Timer -> World -> Bool
timerReady (Timer _ endTime) i = runTime i >= endTime

-------------------------------------------------------
-- Testing
-------------------------------------------------------

data TreeTest = TestInsert GameObject
              | TestUpdate Int Vector3
              | TestRemove Int
              deriving (Show)

instance Arbitrary TreeTest where
    arbitrary = choose (0, 2) >>= \which -> case (which :: Int) of
        0 -> arbitrary >>= return . TestInsert
        1 -> arbitrary >>= \w -> arbitrary >>= \h -> arbitrary >>= \d -> arbitrary >>= \index -> return (TestUpdate index $ Vector3 w h d)
        _ -> arbitrary >>= return . TestRemove

instance Arbitrary GameObject where
    arbitrary = do
        (w,  h,  d)  <- arbitrary
        (px, py, pz) <- arbitrary
        (rx, ry, rz) <- arbitrary
        (sx, sy, sz) <- arbitrary
        return $ GameObject New (Vector3 px py pz) (fromEuler rx ry rz) (Vector3 sx sy sz) (boxCollider w h d) Nothing Nothing []

dynTreeTester :: ((GameObject, DynamicTree) -> Bool) -> [[TreeTest]] -> Bool
dynTreeTester f uss = fst $ foldr updateTest start uss
    where
        start                    = (True, (GameObject New 0 identity 1 Nothing Nothing Nothing [], empty))
        updateTest us (True,  t) = let res = update (foldr test' t us) in (f res, res)
        updateTest _  (False, t) = (False, t)
        test' (TestInsert c)     (g, t) = (gaddChild   c g, t)
        test' (TestRemove i)     (g, t) = (removeChild g i, t)
        test' (TestUpdate i tr)  (g, t)
            | null cs2  = (g, t)
            | otherwise = (gchildren_ cs' g, t)
            where
                cs'        = cs1 ++ (c : tail cs2)
                (cs1, cs2) = splitAt i $ children g
                c = GameObject New tr identity 1 (collider (head cs2)) Nothing Nothing (children (head cs2))

validateIDs :: (GameObject, DynamicTree) -> Bool
validateIDs (g, t) = sort (tids (nodes t) []) == gids && sort nids == gids
    where
        -- trace (show t ++ "\n") $
        tids  Tip           acc = acc
        tids (Leaf _ uid)   acc = uid : acc
        tids (Node _ l r _) acc = tids r $ tids l acc
        (_, nids)               = V.foldl' (\(i, ids) x -> if x == Nothing then (i + 1, ids) else (i + 1, i : ids) ) (0, []) $ nodeData t
        gid' g' acc             = case collider g' of
            Just (SphereCollider (UID c) _ _ _) -> c : acc
            Just (BoxCollider    (UID c) _ _ _) -> c : acc
            _                                   -> acc
        gids                    = sort $ foldChildren gid' [] g

dynTreeTest :: IO ()
dynTreeTest = do
    putStrLn "----------------------------------------------------------------------------------------"
    putStrLn "validateIDs test"
    putStrLn ""
    quickCheckWith (stdArgs { maxSize = 100, maxSuccess = 100 }) $ dynTreeTester validateIDs


-------------------------------------------------------
-- Debug drawing
-------------------------------------------------------

debugDrawCollider :: Collider -> Matrix4x4 -> Matrix4x4 -> Resources -> IO ()
debugDrawCollider (BoxCollider _ t (OBB hs) _) view proj resources = drawMeshWithMaterial (debugDraw green) cubeOutline (view .*. t .*. trsMatrix 0 identity (hs * 2)) proj resources
debugDrawCollider  _                       _         _    _        = return ()

debugDrawAABB :: Color -> AABB -> Matrix4x4 -> Matrix4x4 -> Resources -> IO ()
debugDrawAABB c aabb view proj resources = drawMeshWithMaterial (debugDraw c) cubeOutline (view .*. trsMatrix (center aabb) identity (size aabb)) proj resources

debugDrawDynamicTree :: DynamicTree -> Matrix4x4 -> Matrix4x4 -> Resources -> IO ()
debugDrawDynamicTree tree view proj resources = drawNode (nodes tree)
    where
        drawNode (Node aabb l r _) = debugDrawAABB blue   aabb view proj resources >> drawNode l >> drawNode r
        drawNode (Leaf aabb _)     = debugDrawAABB whiteA aabb view proj resources
        drawNode  Tip              = return ()
