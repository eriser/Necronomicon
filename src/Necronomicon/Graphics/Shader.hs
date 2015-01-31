module Necronomicon.Graphics.Shader (
    Shader(..),
    LoadedShader,
    VertexShader(..),
    FragmentShader(..),
    vert,
    frag,
    compileVert,
    compileFrag,
    shader,
    offset0,
    offsetPtr,
    loadVertexShader,
    loadFragmentShader
    ) where

import           Control.Monad              (unless)
import           Data.Maybe                 (fromMaybe)
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax
import           Necronomicon.Utility
import           Prelude
import           System.IO                  (hPutStrLn, stderr)

import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as C
import           Foreign.Ptr                (Ptr, wordPtrToPtr)
import qualified Graphics.Rendering.OpenGL  as GL
import           Paths_Necronomicon

------------------------------------------------------------------------------------------
-- Shaders
------------------------------------------------------------------------------------------

newtype VertexShader   = VertexShader   {unVertexShader        :: IO GL.Shader}
newtype FragmentShader = FragmentShader {unFragmentShader      :: IO GL.Shader}
data    Shader         = Shader         {key :: Int,loadShader :: IO LoadedShader}
type    LoadedShader   = (GL.Program,[GL.UniformLocation],[GL.AttribLocation])

instance Show Shader where
    show _ = "Shader"

-- loadShader :: GL.ShaderType -> FilePath -> IO GL.Shader
-- loadShader shaderType filePath = BS.readFile filePath >>= loadShaderBS filePath shaderType

loadShaderBS :: FilePath -> GL.ShaderType -> BS.ByteString -> IO GL.Shader
loadShaderBS filePath shaderType src = do
    newShader <- GL.createShader shaderType
    GL.shaderSourceBS newShader GL.$= src
    GL.compileShader  newShader
    printError
    ok      <- GL.get (GL.compileStatus newShader)
    infoLog <- GL.get (GL.shaderInfoLog newShader)
    unless (null infoLog)
        (mapM_ putStrLn ["Shader info log for '" ++ filePath ++ "':", infoLog, ""])
    unless ok $ do
        GL.deleteObjectName newShader
        ioError (userError "shader compilation failed")
    return newShader

loadVertexShader :: FilePath -> VertexShader
loadVertexShader path = VertexShader load
   where
       load = do
           resources <- getDataFileName ""
           putStrLn $ "loadVertexShader: " ++ path
           shaderPath <- BS.readFile $ resources ++ "shaders/" ++ path
           loadShaderBS path GL.VertexShader shaderPath

loadFragmentShader :: FilePath -> FragmentShader
loadFragmentShader path = FragmentShader load
    where
        load = do
            putStrLn $ "loadFragmentShader: " ++ path
            resources <- getDataFileName ""
            shaderPath <- BS.readFile $ resources ++ "shaders/" ++ path
            loadShaderBS path GL.FragmentShader shaderPath

printError :: IO ()
printError = GL.get GL.errors >>= mapM_ (hPutStrLn stderr . ("GL: "++) . show)

vert :: QuasiQuoter
vert = QuasiQuoter (shaderQuoter "compileVert") (error "This quoter has not been defined") (error "This quoter has not been defined") (error "This quoter has not been defined")

frag :: QuasiQuoter
frag = QuasiQuoter (shaderQuoter "compileFrag") (error "This quoter has not been defined") (error "This quoter has not been defined") (error "This quoter has not been defined")

shaderQuoter :: String -> String -> Q Exp
shaderQuoter compileString string = do
    name <- getValueName compileString
    return $ AppE (VarE name) (LitE $ StringL string)

compileVert :: String -> VertexShader
compileVert s = VertexShader   . loadShaderBS "vert" GL.VertexShader   $ C.pack s

compileFrag :: String -> FragmentShader
compileFrag s = FragmentShader . loadShaderBS "frag" GL.FragmentShader $ C.pack s

getValueName :: String -> Q Name
getValueName s = lookupValueName s >>= return . fromMaybe (mkName s)

shader :: String -> [String] -> [String] -> VertexShader -> FragmentShader -> Shader
shader shaderName uniformNames attributeNames vs fs = Shader (hash shaderName) $ do
    putStrLn $ "Compiling shader: " ++ shaderName

    program <- GL.createProgram
    vs'     <- unVertexShader   vs
    fs'     <- unFragmentShader fs

    GL.attachShader    program  vs'
    GL.attachShader    program  fs'
    GL.validateProgram program
    GL.linkProgram     program

    uniforms   <- mapM (GL.get . GL.uniformLocation program) uniformNames
    attributes <- mapM (GL.get . GL.attribLocation  program) attributeNames

    return (program,uniforms,attributes)

-- |Produce a 'Ptr' value to be used as an offset of the given number
-- of bytes.
offsetPtr :: Int -> Ptr a
offsetPtr = wordPtrToPtr . fromIntegral

-- |A zero-offset 'Ptr'.
offset0 :: Ptr a
offset0 = offsetPtr 0


------------------------------------------------------------------------------------------
-- Material
------------------------------------------------------------------------------------------

-- data Material = Material {drawMeshWithMaterial :: Mesh -> Matrix4x4 -> Matrix4x4 -> Resources -> IO ()}

{-
ambientMesh :: [Vector3] -> [Color] -> [Int] -> Mesh
ambientMesh vertices colors indices = Mesh draw
    where
        vertexBuffer = makeBuffer GL.ArrayBuffer           (map realToFrac (posCol vertices colors) :: [GL.GLfloat])
        indexBuffer  = makeBuffer GL.ElementArrayBuffer    (map fromIntegral indices :: [GL.GLuint])
        vertexVad    = GL.VertexArrayDescriptor 3 GL.Float (fromIntegral $ sizeOf (undefined::GL.GLfloat) * 6) offset0
        colorVad     = GL.VertexArrayDescriptor 3 GL.Float (fromIntegral $ sizeOf (undefined::GL.GLfloat) * 6) (offsetPtr $ sizeOf (undefined :: GL.GLfloat) * 3)
        numIndices   = length indices

        draw modelView proj resources = do
            (program,uniforms,attributes) <- getShader resources ambientShader
            GL.currentProgram GL.$= Just program
            bindMatrixUniforms uniforms modelView proj
            bindThenDraw vertexBuffer indexBuffer (zip attributes [vertexVad,colorVad]) numIndices
-}
