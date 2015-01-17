module Necronomicon.Graphics.Text (drawText,
                                   renderFont) where

import           Control.Monad
import           Data.IORef
import           Data.Maybe                                          (fromMaybe)
import           Foreign
import           Foreign.C.String
import           Graphics.Rendering.FreeType.Internal
import           Graphics.Rendering.FreeType.Internal.Bitmap
import           Graphics.Rendering.FreeType.Internal.Face
import qualified Graphics.Rendering.FreeType.Internal.GlyphMetrics   as Metrics
import           Graphics.Rendering.FreeType.Internal.GlyphSlot
import           Graphics.Rendering.FreeType.Internal.Library
import           Graphics.Rendering.FreeType.Internal.PrimitiveTypes
import           Graphics.Rendering.OpenGL                           hiding (bitmap)

import qualified Data.Map                                            as Map

import qualified Necronomicon.Graphics.Color                         as Color (Color (..), white)
import           Necronomicon.Graphics.Model
import qualified Necronomicon.Graphics.Texture                       as NecroTex
import qualified Necronomicon.Linear                                 as Linear
import           Paths_Necronomicon

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

drawText :: String -> Font -> (NecroTex.Texture -> Material) -> Model
drawText = FontRenderer

loadFontAtlas :: Font -> IO LoadedFont
loadFontAtlas font = do
    putStrLn $ "loadFontAtlas: " ++ fontKey font
    fontPath <- getDataFileName ""
    ft <- freeType
    ff <- fontFace ft $ fontPath ++ "fonts/" ++ fontKey font
    runFreeType $ ft_Set_Pixel_Sizes ff (fromIntegral $ fontSize font) 0

    cmetrics <- mapM (getCharMetrics ff) [32..128]
    let (atlasWidth',atlasHeight') = foldr (\metric (w,h) -> (w + charWidth metric + 1, max h (charHeight metric))) (0,0) cmetrics

    atlasTexture <- newBoundTexUnit 0
    rowAlignment Unpack $= 1

    let tsize = TextureSize2D (floor atlasWidth') (floor atlasHeight')
    putStrLn $ "Atlas size: " ++ show tsize
    texImage2D Texture2D NoProxy 0 RGB8 tsize 0 $ PixelData Luminance UnsignedByte nullPtr

    textureFilter   Texture2D   $= ((Linear', Nothing), Linear')
    textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
    textureWrapMode Texture2D T $= (Repeated, ClampToEdge)

    let (_,charMap) = foldr (createCharMap atlasWidth') (0,Map.empty) cmetrics

    mapM_ (addCharToAtlas atlasWidth' ff charMap) [32..128]

    [vertexBuffer] <- genObjectNames 1
    [indexBuffer]  <- genObjectNames 1

    return $ LoadedFont (NecroTex.Texture [] (return atlasTexture)) atlasWidth' atlasHeight' charMap vertexBuffer indexBuffer

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
                           --TODO: This might be causing the artifacts.
                           (TexturePosition2D (round $ charTX charMetric * w) 0)
                           (TextureSize2D     (round $ charWidth charMetric) (round $ charHeight charMetric))
                           (PixelData Luminance UnsignedByte $ buffer bmp)

getCharMetrics :: FT_Face -> Int -> IO CharMetric
getCharMetrics ff char = do
    chNdx  <- ft_Get_Char_Index ff $ fromIntegral $ fromEnum char
    runFreeType $ ft_Load_Glyph ff chNdx 0
    slot   <- peek $ glyph ff
    runFreeType    $ ft_Render_Glyph slot ft_RENDER_MODE_NORMAL
    metric <- peek $ metrics slot
    left   <- peek $ bitmap_left slot
    top    <- peek $ bitmap_top slot
    bmp    <- peek $ bitmap slot
    let charMetric = CharMetric
                     (toEnum char)
                     ((fromIntegral $ Metrics.horiAdvance metric) / 64)
                     ((fromIntegral $ Metrics.vertAdvance metric) / 64)
                     ((fromIntegral $ Metrics.horiBearingX metric) / 64)
                     ((fromIntegral $ Metrics.horiBearingY metric) / 64)
                     (fromIntegral $ width bmp)
                     (fromIntegral $ rows bmp)
                     (fromIntegral left)
                     (fromIntegral top)
                     0
    return charMetric

createCharMap :: Double -> CharMetric -> (Double,Map.Map Char CharMetric) -> (Double,Map.Map Char CharMetric)
createCharMap w charMetric (x,charMap) = (x + charWidth charMetric + 1,Map.insert (character charMetric) (charMetric{charTX = x / w}) charMap)

getFont :: Resources -> Font -> IO LoadedFont
getFont resources font = readIORef (fontsRef resources) >>= \fonts ->
    case Map.lookup (fontKey font) fonts of
        Nothing    -> loadFontAtlas font >>= \font' -> (writeIORef (fontsRef resources) $ Map.insert (fontKey font) font' fonts) >> return font'
        Just font' -> return font'

fontScale :: Double
fontScale = 2 / 1024

--Change dynamic meshes to "load" their buffers the first time, so users don't have to supply them
renderFont :: String -> Font -> (NecroTex.Texture -> Material) -> Linear.Matrix4x4 -> Linear.Matrix4x4 -> Resources -> IO()
renderFont text font material modelView proj resources = do
    loadedFont <- getFont resources font
    let (vertices,colors,uvs,indices,_,_,_) = foldl characterMesh ([],[],[],[],0,0,0) $ fitTextIntoBounds text (1 ,0) (characters loadedFont)
        characterMesh                       = textMesh (characters loadedFont) (atlasWidth loadedFont) (atlasHeight loadedFont)
        fontMesh                            = DynamicMesh (characterVertexBuffer loadedFont) (characterIndexBuffer loadedFont) vertices colors uvs indices
    drawMeshWithMaterial (material $ atlas loadedFont) fontMesh modelView proj resources

textMesh :: Map.Map Char CharMetric ->
            Double ->
            Double ->
            ([Linear.Vector3],[Color.Color],[Linear.Vector2],[Int],Int,Double,Double) ->
            Char ->
            ([Linear.Vector3],[Color.Color],[Linear.Vector2],[Int],Int,Double,Double)
textMesh charMetrics aWidth aHeight (vertices,colors,uvs,indices,count,x,y) char = case char of
    '\n' -> (vertices ,colors ,uvs ,indices ,count    ,0   ,y - aHeight * fontScale)
    _    -> (vertices',colors',uvs',indices',count + 4,x+ax,y)
    where
        charMetric = fromMaybe (CharMetric (toEnum 0) 0 0 0 0 0 0 0 0 0) $ Map.lookup char charMetrics
        w          = charWidth  charMetric * fontScale
        h          = charHeight charMetric * fontScale
        l          = charLeft   charMetric * fontScale
        t          = charTop    charMetric * fontScale
        ax         = advanceX   charMetric * fontScale
        tx         = charTX     charMetric

        vertices'  = Linear.Vector3 (l+x)   (y + t)     0  :
                     Linear.Vector3 (l+x+w) (y + t)     0  :
                     Linear.Vector3 (l+x)   (y + t - h) 0  :
                     Linear.Vector3 (l+x+w) (y + t - h) 0  : vertices
        colors'    = Color.white : Color.white : Color.white : Color.white : colors
        uvs'       = Linear.Vector2 (tx                                 ) 0 :
                     Linear.Vector2 (tx + (charWidth  charMetric) / aWidth) 0 :
                     Linear.Vector2 (tx                                 ) ((charHeight charMetric) / aHeight) :
                     Linear.Vector2 (tx + (charWidth  charMetric) / aWidth) ((charHeight charMetric) / aHeight) : uvs
        indices'   = count + 2 : count + 0 : count + 1 : count + 3 : count + 2 : count + 1 : indices

type TextWord = (String,Double)

emptyWord :: TextWord
emptyWord = ([],0)

fitTextIntoBounds :: String -> (Double,Double) -> Map.Map Char CharMetric -> String
fitTextIntoBounds text (w,_) cmetrics = finalText
    where
        (finalText,_) = foldr (fitWordsIntoBounds (w / fontScale))  emptyWord words'
        (_,words',_)  = foldr (splitCharIntoWords cmetrics) (emptyWord,[],0) text

fitWordsIntoBounds :: Double -> TextWord -> TextWord -> TextWord
fitWordsIntoBounds boundsWidth (word,wordWidth) (text,currentWidth) = if wordWidth + currentWidth < boundsWidth
    then (word ++ text,wordWidth + currentWidth)
    else (word ++ ('\n' : text),0)

splitCharIntoWords :: Map.Map Char CharMetric -> Char -> (TextWord,[TextWord],Double) -> (TextWord,[TextWord],Double)
splitCharIntoWords cmetrics char ((word,wordLength),words',totalLength) = if isWhiteSpace char
    then ( emptyWord , (char : word,wordLength + cAdvance) : words' , totalLength + wordLength + cAdvance)
    else ((char : word,wordLength + cAdvance), words' , totalLength)
    where
        cAdvance = case Map.lookup char cmetrics of
            Nothing -> 0
            Just m  -> advanceX m
        isWhiteSpace c = case c of
            ' '  -> True
            '\n' -> True
            _    -> False

--TODO: Finish text with bounds for chat
-- textMeshWithBounds :: String -> Int -> Int ->
