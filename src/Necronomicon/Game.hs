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

-------------------------------------------------------
-- TODO: Ideas
-- Clockwork world / minature-golem-esque world?
-------------------------------------------------------



-------------------------------------------------------
-- GameObject
-------------------------------------------------------

-- data Transform  = Transform Vector3 Quaternion Vector3 deriving (Show)
data GameObject = GameObject {
    pos      :: Vector3,
    rot      :: Quaternion,
    gscale   :: Vector3,
    collider :: Maybe Collider,
    model    :: Maybe Model,
    camera   :: Maybe Camera,
    gameChildren :: [GameObject]
} deriving (Show)

-------------------------------------------------------
-- GameType
-------------------------------------------------------

class GameType a where
    _gameObject :: a -> GameObject
    gameObject_ :: GameObject -> a -> a
    children    :: a -> [a]
    gchildren_  :: [a] -> a -> a

gameObject :: GameObject
gameObject = GameObject 0 identity 1 Nothing Nothing Nothing []


{-
    implement:
    move, rotate, etc
-}

rotate :: GameType a => a -> Vector3 -> a
rotate gt (Vector3 x y z) = gameObject_ (g{rot = rot g * fromEuler' x y z}) gt
    where
        g = _gameObject gt

-------------------------------------------------------
-- GameObject - Getters / Setters
-------------------------------------------------------

transMat :: GameObject -> Matrix4x4
transMat (GameObject p r s _ _ _ _) = trsMatrix p r s

collider_ :: Collider -> GameObject -> GameObject
collider_ c (GameObject p r s _ m cm cs) = GameObject p r s (Just c) m cm cs

gaddChild :: GameObject -> GameObject -> GameObject
gaddChild g (GameObject p r s c m cm cs) = GameObject p r s c m cm (g : cs)

removeChild :: GameObject -> Int -> GameObject
removeChild (GameObject p r s c m cm cs) n
    | null cs2  = GameObject p r s c m cm cs
    | otherwise = GameObject p r s c m cm $ cs1 ++ tail cs2
    where
        (cs1, cs2) = splitAt n cs

instance GameType GameObject where
    _gameObject                                = id
    gameObject_ g _                            = g
    children       (GameObject _ _ _ _ _ _ gs) = gs
    gchildren_  gs (GameObject p r s c m cm _) = GameObject p r s c m cm gs

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
    | Just (Model mesh mat)             <- model g    = drawMeshWithMaterial mat mesh modelView proj resources >> return newWorld
    | Just (FontRenderer text font mat) <- model g    = renderFont text font mat      modelView proj resources >> return newWorld
    | debug, Just c                     <- collider g = debugDrawCollider c           view      proj resources >> return newWorld
    | otherwise                                       = return newWorld
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
runGame f initg = initWindow >>= \mw -> case mw of
    Nothing -> print "Error starting GLFW." >> return ()
    Just w  -> do
        putStrLn "Starting Necronomicon"

        -- GLFW.setCursorPosCallback   w $ Just $ mousePosEvent   signalState
        -- GLFW.setMouseButtonCallback w $ Just $ mousePressEvent signalState
        -- GLFW.setKeyCallback         w $ Just $ keyPressEvent   signalState
        -- GLFW.setWindowSizeCallback  w $ Just $ dimensionsEvent signalState

        resources   <- newResources
        currentTime <- getCurrentTime
        renderNecronomicon False w resources initg empty currentTime
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

        renderNecronomicon quit window resources g tree runTime'
            | quit      = print "Qutting" >> return ()
            | otherwise = do

                GLFW.pollEvents
                q <- (== GLFW.KeyState'Pressed) <$> GLFW.getKey window GLFW.Key'Escape
                currentTime <- getCurrentTime

                let _           = currentTime - runTime'
                    (g', tree') = update (f g, tree)

                renderGraphicsG window resources True g' g' tree'
                threadDelay $ 16667
                renderNecronomicon q window resources g' tree' currentTime

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
        return $ GameObject (Vector3 px py pz) (fromEuler' rx ry rz) (Vector3 sx sy sz) (boxCollider w h d) Nothing Nothing []

dynTreeTester :: ((GameObject, DynamicTree) -> Bool) -> [[TreeTest]] -> Bool
dynTreeTester f uss = fst $ foldr updateTest start uss
    where
        start                    = (True, (GameObject 0 identity 1 Nothing Nothing Nothing [], empty))
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
                c = GameObject tr identity 1 (collider (head cs2)) Nothing Nothing (children (head cs2))

validateIDs :: (GameObject, DynamicTree) -> Bool
validateIDs (g, t) = sort (tids (nodes t) []) == gids && sort nids == gids
    where
        -- trace (show t ++ "\n") $
        tids  Tip           acc = acc
        tids (Leaf _ uid)   acc = uid : acc
        tids (Node _ l r _) acc = tids r $ tids l acc
        (_, nids)               = V.foldl' (\(i, ids) x -> if x == Nothing then (i + 1, ids) else (i + 1, i : ids) ) (0, []) $ nodeData t
        gid g' acc              = case collider g' of
            Just (SphereCollider (UID c) _ _) -> c : acc
            Just (BoxCollider    (UID c) _ _) -> c : acc
            _                                 -> acc
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

debugDrawCollider :: Collider -> Matrix4x4 -> Matrix4x4 -> Resources -> IO ()
debugDrawCollider (BoxCollider _ t (OBB hs)) view proj resources = drawMeshWithMaterial (debugDraw green) cubeOutline (view .*. t .*. trsMatrix 0 identity (hs * 2)) proj resources
debugDrawCollider  _                       _         _    _      = return ()

debugDrawAABB :: Color -> AABB -> Matrix4x4 -> Matrix4x4 -> Resources -> IO ()
debugDrawAABB c aabb view proj resources = drawMeshWithMaterial (debugDraw c) cubeOutline (view .*. trsMatrix (center aabb) identity (size aabb)) proj resources

debugDrawDynamicTree :: DynamicTree -> Matrix4x4 -> Matrix4x4 -> Resources -> IO ()
debugDrawDynamicTree tree view proj resources = drawNode (nodes tree)
    where
        drawNode (Node aabb l r _) = debugDrawAABB blue   aabb view proj resources >> drawNode l >> drawNode r
        drawNode (Leaf aabb _)     = debugDrawAABB whiteA aabb view proj resources
        drawNode  Tip              = return ()
