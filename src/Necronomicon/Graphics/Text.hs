module Necronomicon.Graphics.Text where

import Prelude    
import Control.Monad
import Graphics.Rendering.OpenGL hiding (bitmap)
import Graphics.Rendering.OpenGL.GL.PixelRectangles.PixelStorage
import Graphics.Rendering.FreeType.Internal
import Graphics.Rendering.FreeType.Internal.PrimitiveTypes
import Graphics.Rendering.FreeType.Internal.Library
import Graphics.Rendering.FreeType.Internal.FaceType
import Graphics.Rendering.FreeType.Internal.Face
import Graphics.Rendering.FreeType.Internal.GlyphSlot
import Foreign
import Foreign.C.String
import Graphics.Rendering.FreeType.Internal.Bitmap
-- import Graphics.Texture.Load
-- import Graphics.Utils
import qualified Graphics.Rendering.FreeType.Internal.BitmapSize as BS

{-
loadCharacter :: FilePath -> Char -> Int -> Int -> IO TextureObject
loadCharacter path char px texUnit = do
    -- FreeType (http://freetype.org/freetype2/docs/tutorial/step1.html)
    ft <- freeType

    -- Get the Ubuntu Mono fontface.
    ff <- fontFace ft path
    runFreeType $ ft_Set_Pixel_Sizes ff (fromIntegral px) 0

    -- Get the unicode char index.
    chNdx <- ft_Get_Char_Index ff $ fromIntegral $ fromEnum char

    -- Load the glyph into freetype memory.
    runFreeType $ ft_Load_Glyph ff chNdx 0

    -- Get the GlyphSlot.
    slot <- peek $ glyph ff

    -- Number of glyphs
    n <- peek $ num_glyphs ff
    putStrLn $ "glyphs:" ++ show n

    fmt <- peek $ format slot
    putStrLn $ "glyph format:" ++ glyphFormatString fmt

    -- This is [] for Ubuntu Mono, but I'm guessing for bitmap
    -- fonts this would be populated with the different font
    -- sizes.
    putStr "Sizes:"
    numSizes <- peek $ num_fixed_sizes ff
    sizesPtr <- peek $ available_sizes ff
    sizes <- forM [0 .. numSizes-1] $ \i ->
        peek $ sizesPtr `plusPtr` fromIntegral i :: IO BS.FT_Bitmap_Size
    print sizes

    l <- peek $ bitmap_left slot
    t <- peek $ bitmap_top slot
    putStrLn $ concat [ "left:"
                      , show l
                      , "\ntop:"
                      , show t
                      ]

    runFreeType $ ft_Render_Glyph slot ft_RENDER_MODE_NORMAL

    -- Get the char bitmap.
    bmp <- peek $ bitmap slot
    putStrLn $ concat ["width:"
                      , show $ width bmp
                      , " rows:"
                      , show $ rows bmp
                      , " pitch:"
                      , show $ pitch bmp
                      , " num_grays:"
                      , show $ num_grays bmp
                      , " pixel_mode:"
                      , show $ pixel_mode bmp
                      , " palette_mode:"
                      , show $ palette_mode bmp
                      ]

    let w  = fromIntegral $ width bmp
        h  = fromIntegral $ rows bmp
        w' = fromIntegral w
        h' = fromIntegral h

    -- Set the texture params on our bound texture.
    texture Texture2D $= Enabled

    -- Set the alignment to 1 byte.
    rowAlignment Unpack $= 1

    -- Generate an opengl texture.
    tex <- newBoundTexUnit texUnit
    printError

    putStrLn "Buffering glyph bitmap into texture."
    texImage2D
        Texture2D
        NoProxy
        0
        R8
        (TextureSize2D w' h')
        0
        (PixelData Red UnsignedByte $ buffer bmp)
    printError

    putStrLn "Texture loaded."
    textureFilter   Texture2D   $= ((Linear', Nothing), Linear')
    textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
    textureWrapMode Texture2D T $= (Repeated, ClampToEdge)

    return tex
-}

runFreeType :: IO FT_Error -> IO ()
runFreeType m = do
    r <- m
    unless (r == 0) $ fail $ "FreeType Error:" ++ show r

freeType :: IO FT_Library
freeType = alloca $ \p -> do
    runFreeType $ ft_Init_FreeType p
    peek p

fontFace :: FT_Library -> FilePath -> IO FT_Face
fontFace ft fp = withCString fp $ \str ->
    alloca $ \ptr -> do
        runFreeType $ ft_New_Face ft str 0 ptr
        peek ptr

