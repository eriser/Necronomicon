module Necronomicon.Util.TGA(readTGA,
                             writeTGA,
                             TGAData(..),
                             Picture(..),
                             loadTextureFromTGA) where

import Data.Word
import Data.Bits
import Data.Int
import qualified Data.ByteString as B

import qualified Graphics.Rendering.OpenGL as GL
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Data.Word (Word8(..), Word16)
import Data.ByteString.Internal (ByteString(..),toForeignPtr)
-- import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
-- import Foreign.Ptr (Ptr, plusPtr, castPtr)

-- see http://en.wikipedia.org/wiki/Truevision_TGA
-- or  http://de.wikipedia.org/wiki/Targa_Image_File

data Picture = RGB24 ImageData |
               RGB32 ImageData |
               PaletteUncompressed Palette ImageData | -- not used
               MonoUncompressed ImageData -- not used
               --  PaletteRLE
               --  RGB24RLE

type Palette = B.ByteString
type ImageData = B.ByteString

data TGAData = TGAData {
                 picId :: B.ByteString, -- a String is not appropriate, since Char is not Word8 and ASCII is expected here
                 picture :: Picture,
                 xcoord :: Int,
                 ycoord :: Int,
                 width :: Int,
                 height :: Int
               }

readTGA :: String -> IO TGAData
readTGA fileName = do bs <- B.readFile fileName
                      return (TGAData (readId bs) -- picId
                                      (readPicture bs) -- picture
                                      ((fi bs 8 )+(shiftL (fi bs 9)  8)) -- xcoord  (normally not used)
                                      ((fi bs 10)+(shiftL (fi bs 11) 8)) -- ycoord  (normally not used)
                                      ((fi bs 12)+(shiftL (fi bs 13) 8))   -- width
                                      ((fi bs 14)+(shiftL (fi bs 15) 8)) ) -- height

fi :: B.ByteString -> Int -> Int
fi bs i = fromIntegral $ B.index bs i

readId bs = B.take len (B.drop 17 bs)
  where len = fromIntegral (B.index bs 0)

readPicture bs | ((B.index bs 2) == 2) && ((B.index bs 16) == 24) = RGB24 (B.drop (17+id_len+pal_len+1) bs)
               | ((B.index bs 2) == 2) && ((B.index bs 16) == 32) = RGB32 (B.drop (17+id_len+pal_len+1) bs)
  where id_len = fromIntegral (B.index bs 0)
        pal_len = fromIntegral ((B.index bs 5) + (shiftL (B.index bs 6) 8))

writeTGA :: String -> TGAData -> IO ()
writeTGA fileName tgaData | sizeCorrect tgaData = B.writeFile fileName $ (header tgaData) `B.append` (body tgaData)

sizeCorrect (TGAData _ (RGB24 p) _ _ x y) | (B.length p) == (fromIntegral (x*y*3)) = True
                                          | otherwise = error ( "error in tga output: RGB24 Data length=" ++ show (B.length p) ++
                                                                ", but "++show x ++"*"++ show y ++ "*3="++ (show (x*y*3)) )
sizeCorrect (TGAData _ (RGB32 p) _ _ x y) | (B.length p) == (fromIntegral (x*y*4)) = True
                                          | otherwise = error ( "error in tga output: RGB32 Data length=" ++ show (B.length p) ++
                                                                ", but "++show x ++"*"++ show y ++ "*4="++ (show (x*y*4)) )

header :: TGAData -> B.ByteString
header file = B.pack ([(fromIntegral (B.length (picId file))) :: Word8] ++  -- index 0
                       [hasPalette p] ++                                    -- index 1
                       [ptype p] ++                                         -- index 2
                       [0::Word8] ++ [0::Word8] ++    -- offset (not used)  -- index 3,4
                       (palette_len p) ++                                   -- index 5,6
                       [palette_entry_len p] ++                             -- index 7
                       (toW8 (xcoord file)) ++ (toW8 (ycoord file)) ++      -- index 8,9,   10,11
                       (toW8 (width file)) ++ (toW8 (height file)) ++       -- index 12,13  14,15
                       [pixelBits p] ++                                     -- index 16
                       [32::Word8]) -- picture attribute byte
  where p = (picture file)

hasPalette (PaletteUncompressed _ _) = 1 :: Word8
hasPalette _ = 0 :: Word8

pixelBits (RGB24 _) = 24 :: Word8
pixelBits (RGB32 _) = 32 :: Word8

ptype (PaletteUncompressed _ _) = 1 :: Word8
ptype (RGB24 _) = 2 :: Word8
ptype (RGB32 _) = 2 :: Word8
ptype (MonoUncompressed _) = 3 :: Word8

palette_len (PaletteUncompressed palette _) = toW8 $ (toEnum . fromEnum) (B.length palette)
palette_len _ = [0 :: Word8, 0 :: Word8]

toW8 :: Int -> [Word8]
toW8 i = [(fromIntegral (i .&. 255)) :: Word8, (fromIntegral (shiftR i 8)) :: Word8]

palette_entry_len (RGB24 _) = 0 :: Word8
palette_entry_len (RGB32 _) = 0 :: Word8



body :: TGAData -> B.ByteString
body file = (picId file) `B.append` (getPalette (picture file)) `B.append` (getData (picture file))

getPalette (PaletteUncompressed p _) = p
getPalette _ = B.empty

getData (RGB32 d) = d
getData (RGB24 d) = d
getData (PaletteUncompressed _ d) = d
getData (MonoUncompressed d) = d

withPixels b m = aux . toForeignPtr $ b
    where aux (fp,o,_) = withForeignPtr fp $ \p -> m (plusPtr p o)

newBoundTexUnit :: Int -> IO GL.TextureObject
newBoundTexUnit u = do
    [tex] <- GL.genObjectNames 1
    GL.texture        GL.Texture2D GL.$= GL.Enabled
    GL.activeTexture               GL.$= GL.TextureUnit (fromIntegral u)
    GL.textureBinding GL.Texture2D GL.$= Just tex
    return tex

loadTextureFromTGA :: String -> IO GL.TextureObject
loadTextureFromTGA path = do
    -- print "Reading TGA data."
    tex <- newBoundTexUnit 0
    tga <- readTGA path
    -- print "Loading to texture."
    let size = GL.TextureSize2D (fromIntegral (width tga)) (fromIntegral  (height tga))
    case picture tga of
        RGB32 p -> withPixels p (GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGB8 size 0 . GL.PixelData GL.RGB GL.UnsignedByte)
        RGB24 p -> withPixels p (GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGB8 size 0 . GL.PixelData GL.RGB GL.UnsignedByte)
    GL.textureFilter   GL.Texture2D      GL.$= ((GL.Linear', Nothing), GL.Linear')
    GL.textureWrapMode GL.Texture2D GL.S GL.$= (GL.Repeated, GL.ClampToEdge)
    GL.textureWrapMode GL.Texture2D GL.T GL.$= (GL.Repeated, GL.ClampToEdge)
    print "Texture complete."
    return tex



