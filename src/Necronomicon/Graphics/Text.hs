module Necronomicon.Graphics.Text (drawText,
                                   renderFont,
                                   font) where

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
import qualified Graphics.Rendering.FreeType.Internal.GlyphMetrics as Metrics
import Foreign
import Foreign.C.String
import Graphics.Rendering.FreeType.Internal.Bitmap
import Data.Bits (shift)
import Data.IORef

import qualified Graphics.Rendering.FreeType.Internal.BitmapSize as BS
import qualified Control.Exception as E
import qualified Data.Map as Map

import qualified Necronomicon.Linear as Linear
import qualified Necronomicon.Graphics.Mesh as Mesh (rect,loadDynamicMesh)
import qualified Necronomicon.Graphics.Texture as NecroTex
import Necronomicon.Graphics.Model
import qualified Necronomicon.Graphics.Color as Color (Color(..),white)

newBoundTexUnit :: Int -> IO TextureObject
newBoundTexUnit u = do
    [tex] <- genObjectNames 1
    texture Texture2D        $= Enabled
    activeTexture            $= TextureUnit (fromIntegral u)
    textureBinding Texture2D $= Just tex
    return tex

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

glyphFormatString :: FT_Glyph_Format -> String
glyphFormatString fmt
    | fmt == ft_GLYPH_FORMAT_COMPOSITE = "ft_GLYPH_FORMAT_COMPOSITE"
    | fmt == ft_GLYPH_FORMAT_OUTLINE = "ft_GLYPH_FORMAT_OUTLINE"
    | fmt == ft_GLYPH_FORMAT_PLOTTER = "ft_GLYPH_FORMAT_PLOTTER"
    | fmt == ft_GLYPH_FORMAT_BITMAP = "ft_GLYPH_FORMAT_BITMAP"
    | otherwise = "ft_GLYPH_FORMAT_NONE"


font :: String -> Int -> Font
font path size = Font path size

drawText :: String -> Font -> (NecroTex.Texture -> Material) -> Model
drawText = FontRenderer

loadFontAtlas :: Font -> IO LoadedFont
loadFontAtlas font = do
    ft <- freeType
    ff <- fontFace ft $ fontKey font
    runFreeType $ ft_Set_Pixel_Sizes ff (fromIntegral $ fontSize font) 0

    metrics <- mapM (getCharMetrics ff) [32..128] --[32..128]
    let (atlasWidth,atlasHeight) = foldr (\metric (w,h) -> (w + advanceX metric,max h (charHeight metric))) (0,0) metrics

    atlasTexture <- newBoundTexUnit 0
    rowAlignment Unpack $= 1

    let size = TextureSize2D (floor atlasWidth) (floor atlasHeight)
    putStrLn $ "Atlas size: " ++ show size
    texImage2D Texture2D NoProxy 0 RGB8 size 0 $ PixelData Luminance UnsignedByte nullPtr

    textureFilter   Texture2D   $= ((Linear', Nothing), Linear')
    textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
    textureWrapMode Texture2D T $= (Repeated, ClampToEdge)

    let (_,charMap) = foldr (createCharMap atlasWidth) (0,Map.empty) metrics

    mapM_ (addCharToAtlas atlasWidth ff charMap) [32..128]

    [vertexBuffer] <- genObjectNames 1
    [indexBuffer]  <- genObjectNames 1

    return $ LoadedFont (NecroTex.Texture [] (return atlasTexture)) atlasWidth atlasHeight charMap vertexBuffer indexBuffer

fontAtlasTexture :: Font -> NecroTex.Texture
fontAtlasTexture font = NecroTex.Texture (fontKey font) $ do
    loadedFont <- loadFontAtlas font
    let (NecroTex.Texture _ tex) = atlas loadedFont
    tex

addCharToAtlas :: Double -> FT_Face -> Map.Map Char CharMetric -> Int -> IO()
addCharToAtlas w ff charMap char = do
    chNdx  <- ft_Get_Char_Index ff $ fromIntegral $ fromEnum char
    runFreeType    $ ft_Load_Glyph ff chNdx 0
    slot   <- peek $ glyph ff
    runFreeType    $ ft_Render_Glyph slot ft_RENDER_MODE_NORMAL
    bmp    <- peek $ bitmap slot
    case Map.lookup (toEnum char) charMap of
        Nothing         -> return ()
        Just charMetric -> texSubImage2D
                           Texture2D
                           0
                           (TexturePosition2D (floor $ charTX charMetric * w) 0)
                           (TextureSize2D     (floor $ charWidth charMetric) (floor $ charHeight charMetric))
                           (PixelData Luminance UnsignedByte $ buffer bmp)

getCharMetrics :: FT_Face -> Int -> IO CharMetric
getCharMetrics ff char = do
    chNdx  <- ft_Get_Char_Index ff $ fromIntegral $ fromEnum char
    runFreeType $ ft_Load_Glyph ff chNdx 0
    slot   <- peek $ glyph ff

    --not sure if this is necessary??
    runFreeType    $ ft_Render_Glyph slot ft_RENDER_MODE_NORMAL
    
    metric <- peek $ metrics slot
    left   <- peek $ bitmap_left slot
    top    <- peek $ bitmap_top slot
    let charMetric = CharMetric
                     (toEnum char)
                     (fromIntegral (shift (Metrics.horiAdvance  metric) (-6)))
                     (fromIntegral (shift (Metrics.vertAdvance  metric) (-6)))
                     (fromIntegral (shift (Metrics.horiBearingX metric) (-6)))
                     (fromIntegral (shift (Metrics.horiBearingY metric) (-6)))
                     (fromIntegral (shift (Metrics.width        metric) (-6)))
                     (fromIntegral (shift (Metrics.height       metric) (-6)))
                     (fromIntegral left)
                     (fromIntegral top)
                     0
    -- print charMetric
    return charMetric

createCharMap :: Double -> CharMetric -> (Double,Map.Map Char CharMetric) -> (Double,Map.Map Char CharMetric)
createCharMap w charMetric (x,charMap) = (x + charWidth charMetric,Map.insert (character charMetric) (charMetric{charTX = x / w}) charMap)

getFont :: Resources -> Font -> IO LoadedFont
getFont resources font = readIORef (fontsRef resources) >>= \fonts ->
    case Map.lookup (fontKey font) fonts of
        Nothing    -> loadFontAtlas font >>= \font' -> (writeIORef (fontsRef resources) $ Map.insert (fontKey font) font' fonts) >> return font'
        Just font' -> return font'

renderFont :: String -> Font -> (NecroTex.Texture -> Material) -> Linear.Matrix4x4 -> Linear.Matrix4x4 -> Resources -> IO()
renderFont text font material modelView proj resources = do
    loadedFont <- getFont resources font
    let (vertices,colors,uvs,indices,_,_) = foldl (textMesh (characters loadedFont) ((fromIntegral $ fontSize font) * 1) (atlasWidth loadedFont) (atlasHeight loadedFont)) ([],[],[],[],0,0) text
        fontMesh                          = Mesh [] $ Mesh.loadDynamicMesh (characterVertexBuffer loadedFont) (characterIndexBuffer loadedFont) vertices colors uvs indices
    drawMeshWithMaterial (material $ atlas loadedFont) fontMesh modelView proj resources

textMesh :: Map.Map Char CharMetric ->
            Double ->
            Double ->
            Double ->
            ([Linear.Vector3],[Color.Color],[Linear.Vector2],[Int],Int,Double) ->
            Char ->
            ([Linear.Vector3],[Color.Color],[Linear.Vector2],[Int],Int,Double)
textMesh charMetrics size atlasWidth atlasHeight (vertices,colors,uvs,indices,count,x) char = (vertices',colors',uvs',indices',count + 4,x+advanceX charMetric)
    where
        charMetric = case Map.lookup char charMetrics of
            Nothing -> CharMetric (toEnum 0) 0 0 0 0 0 0 0 0 0
            Just cm -> cm
        w          = charWidth  charMetric
        h          = charHeight charMetric
        t          = charTop    charMetric
        tx         = charTX     charMetric
        bx         = bearingX   charMetric
        by         = bearingY   charMetric

        vertices'  = Linear.Vector3 ((x+bx)   / size) ((t-by)   / size) 0  :
                     Linear.Vector3 ((x+bx+w) / size) ((t-by)   / size) 0  :
                     Linear.Vector3 ((x+bx)   / size) ((t-by+h) / size) 0  :
                     Linear.Vector3 ((x+bx+w) / size) ((t-by+h) / size) 0  :
                     vertices
        colors'    = Color.white : Color.white : Color.white : Color.white : colors
        uvs'       = Linear.Vector2 (tx                 ) (h / atlasHeight) :
                     Linear.Vector2 (tx + w / atlasWidth) (h / atlasHeight) :
                     Linear.Vector2 (tx                 ) 0                 :
                     Linear.Vector2 (tx + w / atlasWidth) 0                 :
                     uvs
        indices'   = count + 2 : count + 0 : count + 1 : count + 3 : count + 2 : count + 1 : indices

