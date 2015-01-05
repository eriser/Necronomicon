{-# LANGUAGE ScopedTypeVariables #-}
module Necronomicon.Graphics.BufferObject where

import Prelude
import Data.Word (Word32)
import qualified Graphics.Rendering.OpenGL as GL
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types
-- import Data.Array.Storable
import Data.ByteString (ByteString, useAsCStringLen)

makeBuffer :: (Storable a) => GL.BufferTarget -> [a] -> IO GL.BufferObject
makeBuffer target elems = makeBufferWithLength target (length elems) elems

makeBufferWithLength :: (Storable a) => GL.BufferTarget -> Int -> [a] -> IO GL.BufferObject
makeBufferWithLength target len elems = do
    [buffer] <- GL.genObjectNames 1
    GL.bindBuffer target GL.$= Just buffer
    -- arr <- newListArray (0, len - 1) elems
    -- withStorableArray arr $ \ptr -> 
        -- GL.bufferData target GL.$= (n, ptr, GL.StaticDraw)
    -- let v = Vec.fromList elems
    -- Vec.unsafeWith v $ \ptr -> GL.bufferData target GL.$= (n, ptr, GL.StaticDraw)
    withArray elems $ \ptr -> GL.bufferData target GL.$= (n, ptr, GL.StaticDraw)
    return buffer
    where
        n = fromIntegral $ len * sizeOf (head elems)

