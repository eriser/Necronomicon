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
import Necronomicon.Graphics
import Necronomicon.Utility              (getCurrentTime)

-------------------------------------------------------
-- TODO: Ideas
-- Clockwork world / minature-golem-esque world?
-------------------------------------------------------



-------------------------------------------------------
-- GameObject
-------------------------------------------------------

data UID        = UID Int | New deriving (Show)
data Transform  = Transform Vector3 Quaternion Vector3 deriving (Show)
data GameObject = GameObject Transform (Maybe Collider) (Maybe Model) (Maybe Camera) [GameObject] deriving (Show)


-------------------------------------------------------
-- GameObject - Getters / Setters
-------------------------------------------------------

transform :: GameObject -> Transform
transform (GameObject t _ _ _ _) = t

transMat :: GameObject -> Matrix4x4
transMat (GameObject (Transform p r s) _ _ _ _) = trsMatrix p r s

children :: GameObject -> [GameObject]
children (GameObject _ _ _ _ cs) = cs

gchildren_ :: [GameObject] -> GameObject -> GameObject
gchildren_ cs (GameObject t c m cm _) = GameObject t c m cm cs

collider :: GameObject -> Maybe Collider
collider (GameObject _ c _ _ _) = c

collider_ :: Collider -> GameObject -> GameObject
collider_ c (GameObject t _ m cm cs) = GameObject t (Just c) m cm cs

model :: GameObject -> Maybe Model
model (GameObject _ _ m _ _) = m

camera :: GameObject -> Maybe Camera
camera (GameObject _ _ _ cm _) = cm

gaddChild :: GameObject -> GameObject -> GameObject
gaddChild g (GameObject t c m cm cs) = GameObject t c m cm (g : cs)

removeChild :: GameObject -> Int -> GameObject
removeChild (GameObject t c m cm cs) n
    | null cs2  = GameObject t c m cm cs
    | otherwise = GameObject t c m cm $ cs1 ++ tail cs2
    where
        (cs1, cs2) = splitAt n cs

-------------------------------------------------------
-- GameObject - Folding / Mapping
-------------------------------------------------------

foldChildren :: (GameObject -> a -> a) -> a -> GameObject -> a
foldChildren f acc g = foldr (\c acc' -> foldChildren f acc' c) (f g acc) (children g)

mapFold :: ((GameObject, a) -> (GameObject, a)) -> (GameObject, a) -> (GameObject, a)
mapFold f gacc = (gchildren_ gcs g, acc')
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
-- Colliders
-------------------------------------------------------

data Collider = SphereCollider UID Sphere
              | BoxCollider    UID Matrix4x4
              deriving (Show)

colliderID :: Collider -> UID
colliderID (SphereCollider uid _) = uid
colliderID (BoxCollider    uid _) = uid

colliderID_ :: UID -> Collider -> Collider
colliderID_ uid (SphereCollider _ x) = SphereCollider uid x
colliderID_ uid (BoxCollider    _ x) = BoxCollider    uid x

boxCollider :: Double -> Double -> Double -> Maybe Collider
boxCollider w h d = Just $ BoxCollider New $ trsMatrix 0 identity (Vector3 w h d)

cubeVertices :: [Vector3]
cubeVertices = [Vector3 (-0.5) (-0.5)   0.5,
                Vector3   0.5  (-0.5)   0.5,
                Vector3 (-0.5)   0.5    0.5,
                Vector3   0.5    0.5    0.5,
                Vector3 (-0.5) (-0.5) (-0.5),
                Vector3   0.5  (-0.5) (-0.5),
                Vector3 (-0.5)   0.5  (-0.5),
                Vector3   0.5    0.5  (-0.5)]

calcAABB :: Matrix4x4 -> Collider -> AABB
calcAABB mat (BoxCollider _ bmat) = aabbFromPoints $ map (.*. (mat .*. bmat)) cubeVertices
calcAABB  _   _                   = 0

-------------------------------------------------------
-- Components
-------------------------------------------------------

data Component = Graphics Model
               | CameraComponent Camera
--                    | Audio    String Double Bool
--                    | Light    LightType
--                    | Timer    Double Double


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
            | UID uid <- cid = (g , (nodeListCons (caabb, uid) d,               il, t), newWorld)
            | otherwise      = (g', (nodeListCons (caabb,   i) d, (caabb', i) : il, t{freeList = fl}), newWorld)
            where
                cid      = colliderID  col
                caabb    = calcAABB    newWorld col
                caabb'   = enlargeAABB caabb
                (i : fl) = freeList t
                g'       = collider_ (colliderID_ (UID i) col) g

-------------------------------------------------------
-- Rendering
-------------------------------------------------------

drawGame :: Matrix4x4 -> Matrix4x4 -> Matrix4x4 -> Resources -> Bool -> GameObject -> IO ()
drawGame world view proj resources debug g = do
    newWorld <- drawG world view proj resources debug  g
    mapM_ (drawGame newWorld view proj resources debug) (children g)

drawG :: Matrix4x4 -> Matrix4x4 -> Matrix4x4 -> Resources -> Bool -> GameObject -> IO Matrix4x4
drawG world view proj resources debug g
    | Just (Model mesh mat)             <- model g    = drawMeshWithMaterial mat mesh modelView proj resources >> return newWorld
    | Just (FontRenderer text font mat) <- model g    = renderFont text font mat      modelView proj resources >> return newWorld
    | debug, Just (BoxCollider _ bm)    <- collider g = debugDrawBoxCollider bm       modelView proj resources >> return newWorld
    | otherwise                                       = return newWorld
    where
        newWorld  = world .*. transMat g
        modelView = view  .*. newWorld

--From Camera.hs
renderCameraG :: (Int,Int) -> Matrix4x4 -> GameObject -> Resources -> Bool -> GameObject -> DynamicTree -> IO Matrix4x4
renderCameraG (w,h) view scene resources debug so t = case camera so of
    Nothing -> return newView
    Just c  -> do
        let  ratio    = fromIntegral w / fromIntegral h
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
                drawGame identity4 (invert newView) (perspMatrix (_fov c) ratio (_near c) (_far c)) resources debug scene
                if debug then debugDrawDynamicTree t (invert newView) (perspMatrix (_fov c) ratio (_near c) (_far c)) resources else return ()

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
            let (Material mdraw) = postRenderMaterial postFX (Texture [] . return .GL.TextureObject $ postRenderTex postFX)
            mdraw (rect 1 1) identity4 (orthoMatrix 0 1 0 1 (-1) 1) resources

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

runGame :: (GameObject -> GameObject) -> GameObject -> IO()
runGame f g = initWindow >>= \mw -> case mw of
    Nothing -> print "Error starting GLFW." >> return ()
    Just w  -> do
        putStrLn "Starting Necronomicon"

        -- GLFW.setCursorPosCallback   w $ Just $ mousePosEvent   signalState
        -- GLFW.setMouseButtonCallback w $ Just $ mousePressEvent signalState
        -- GLFW.setKeyCallback         w $ Just $ keyPressEvent   signalState
        -- GLFW.setWindowSizeCallback  w $ Just $ dimensionsEvent signalState

        resources <- newResources
        renderNecronomicon False w resources g empty 0
    where
        --event callbacks
        -- mousePressEvent state _ _ GLFW.MouseButtonState'Released _ = atomically $ writeTChan (mouseButtonBuffer state) $ False
        -- mousePressEvent state _ _ GLFW.MouseButtonState'Pressed  _ = atomically $ writeTChan (mouseButtonBuffer state) $ True
        -- dimensionsEvent state _ x y = writeToSignal (dimensionsSignal state) $ Vector2 (fromIntegral x) (fromIntegral y)
        -- keyPressEvent   state _ k _ GLFW.KeyState'Pressed  _       = do
            -- atomically $ writeTChan (keySignalBuffer state)   (glfwKeyToEventKey k,True)
            -- atomically $ writeTChan (keysPressedBuffer state) (glfwKeyToEventKey k)
        -- keyPressEvent   state _ k _ GLFW.KeyState'Released _       = atomically $ writeTChan (keySignalBuffer state) (glfwKeyToEventKey k,False)
        -- keyPressEvent   _ _ _ _ _ _                            = return ()
        --
        -- mousePosEvent state w x y = do
        --     (wx,wy) <- GLFW.getWindowSize w
        --     let pos = (x / fromIntegral wx,y / fromIntegral wy)
        --     writeToSignal (mouseSignal state) pos

        renderNecronomicon quit window resources g' tree runTime'
            | quit      = print "Qutting" >> return ()
            | otherwise = do

                GLFW.pollEvents
                q <- (== GLFW.KeyState'Pressed) <$> GLFW.getKey window GLFW.Key'Escape
                currentTime <- getCurrentTime

                let _        = currentTime - runTime'
                    (g'', tree') = update (f g', tree)

                renderGraphicsG window resources True g'' g'' tree'
                threadDelay $ 16667
                renderNecronomicon q window resources g'' tree' currentTime

-------------------------------------------------------
-- Testing
-------------------------------------------------------

data TreeTest = TestInsert GameObject
              | TestUpdate Int Matrix4x4
              | TestRemove Int
              deriving (Show)

instance Arbitrary TreeTest where
    arbitrary = choose (0, 2) >>= \which -> case (which :: Int) of
        0 -> arbitrary >>= return . TestInsert
        1 -> arbitrary >>= \w -> arbitrary >>= \h -> arbitrary >>= \d -> arbitrary >>= \index -> return (TestUpdate index $ trsMatrix 0 identity (Vector3 w h d))
        _ -> arbitrary >>= return . TestRemove

instance Arbitrary GameObject where
    arbitrary = do
        (w,  h,  d)  <- arbitrary
        (px, py, pz) <- arbitrary
        (rx, ry, rz) <- arbitrary
        (sx, sy, sz) <- arbitrary
        return $ GameObject (Transform (Vector3 px py pz) (fromEuler' rx ry rz) (Vector3 sx sy sz)) (Just $ BoxCollider New $ trsMatrix 0 identity (Vector3 w h d)) Nothing Nothing []

dynTreeTester :: ((GameObject, DynamicTree) -> Bool) -> [[TreeTest]] -> Bool
dynTreeTester f uss = fst $ foldr updateTest start uss
    where
        start                    = (True, (GameObject (Transform 0 identity 1) Nothing Nothing Nothing [], empty))
        updateTest us (True,  t) = let res = update (foldr test' t us) in (f res, res)
        updateTest _  (False, t) = (False, t)
        test' (TestInsert c)     (g, t) = (gaddChild   c g, t)
        test' (TestRemove i)     (g, t) = (removeChild g i, t)
        test' (TestUpdate i bm)  (g, t)
            | null cs2  = (g, t)
            | otherwise = (gchildren_ cs' g, t)
            where
                cs'        = cs1 ++ (c : tail cs2)
                (cs1, cs2) = splitAt i $ children g
                c = case head cs2 of
                    (GameObject tr (Just (BoxCollider    uid _)) _ _ gs) -> GameObject tr (Just $ BoxCollider uid bm) Nothing Nothing gs
                    (GameObject tr (Just (SphereCollider uid _)) _ _ gs) -> GameObject tr (Just $ BoxCollider uid bm) Nothing Nothing gs
                    (GameObject tr  _                            _ _ gs) -> GameObject tr (Just $ BoxCollider New bm) Nothing Nothing gs

validateIDs :: (GameObject, DynamicTree) -> Bool
validateIDs (g, t) = sort (tids (nodes t) []) == gids && sort nids == gids
    where
        -- trace (show t ++ "\n") $
        tids  Tip           acc = acc
        tids (Leaf _ uid)   acc = uid : acc
        tids (Node _ l r _) acc = tids r $ tids l acc
        (_, nids)               = V.foldl' (\(i, ids) x -> if x == Nothing then (i + 1, ids) else (i + 1, i : ids) ) (0, []) $ nodeData t
        gid g' acc              = case collider g' of
            Just (SphereCollider (UID c) _) -> c : acc
            Just (BoxCollider    (UID c) _) -> c : acc
            _                               -> acc
        gids                    = sort $ foldChildren gid [] g

dynTreeTest :: IO ()
dynTreeTest = do
    putStrLn "----------------------------------------------------------------------------------------"
    putStrLn "validateIDs test"
    putStrLn ""
    quickCheckWith (stdArgs { maxSize = 100, maxSuccess = 100 }) $ dynTreeTester validateIDs


-------------------------------------------------------
-- Debug drawing
-------------------------------------------------------

debugDrawBoxCollider :: Matrix4x4 -> Matrix4x4 -> Matrix4x4 -> Resources -> IO ()
debugDrawBoxCollider bm modelView proj resources = drawMeshWithMaterial (debugDraw green) cubeOutline (modelView .*. bm) proj resources

debugDrawAABB :: AABB -> Matrix4x4 -> Matrix4x4 -> Resources -> IO ()
debugDrawAABB aabb view proj resources = drawMeshWithMaterial (debugDraw whiteA) cubeOutline (view .*. trsMatrix (center aabb) identity (size aabb)) proj resources

debugDrawDynamicTree :: DynamicTree -> Matrix4x4 -> Matrix4x4 -> Resources -> IO ()
debugDrawDynamicTree tree view proj resources = do
    -- print tree
    putStrLn "debugDrawDynamicTree"
    drawNode (nodes tree)
    where
        drawNode (Node aabb l r _) = print aabb >> debugDrawAABB aabb view proj resources >> drawNode l >> drawNode r
        drawNode (Leaf aabb _)     = print aabb >> debugDrawAABB aabb view proj resources
        drawNode  Tip              = return ()
