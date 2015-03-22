{-# LANGUAGE ScopedTypeVariables #-}
module Necronomicon.Graphics.BufferObject where

import Prelude
import Foreign.Marshal.Array
import Foreign.Storable

import qualified Graphics.Rendering.OpenGL as GL

makeBuffer :: (Storable a) => GL.BufferTarget -> [a] -> IO GL.BufferObject
makeBuffer target elems = makeBufferWithLength target (length elems) elems

makeBufferWithLength :: (Storable a) => GL.BufferTarget -> Int -> [a] -> IO GL.BufferObject
makeBufferWithLength target len elems = do
    -- print "makeBuffer"
    [buffer] <- GL.genObjectNames 1
    GL.bindBuffer target GL.$= Just buffer
    withArray elems $ \ptr -> GL.bufferData target GL.$= (n, ptr, GL.StaticDraw)
    return buffer
    where
        n = fromIntegral $ len * sizeOf (head elems)

makeDynamicBuffer :: (Storable a) => GL.BufferObject -> GL.BufferTarget -> [a] -> IO GL.BufferObject
makeDynamicBuffer buffer target elems = do
    -- print "makeDynamicBuffer"
    GL.bindBuffer target GL.$= Just buffer
    withArray elems $ \ptr -> GL.bufferData target GL.$= (n, ptr, GL.DynamicDraw)
    return buffer
    where
        len = length elems
        n   = fromIntegral $ len * sizeOf (head elems)

genDynMeshBuffers :: IO (GL.BufferObject,GL.BufferObject)
genDynMeshBuffers = do
    vertexBuffer:_ <- GL.genObjectNames 1
    indexBuffer :_ <- GL.genObjectNames 1
    return (vertexBuffer,indexBuffer)


    -- array <- peekArray 512 $ advancePtr outBusBuffers (512 * index)
    -- withArray elems $ \ptr -> GL.bufferData target GL.$= (n, ptr, GL.StaticDraw)
    -- return buffer
    --

-- audioBuffer :: Int -> Signal [Double]
-- audioBuffer index = Signal $ \_ -> if index < 8
        -- then return processState
        -- else return $ \_ -> return $ NoChange []
    -- where
        -- processState _ = do
            -- array <- peekArray 512 $ advancePtr outBusBuffers (512 * index)
            -- return $ Change $ map realToFrac array
